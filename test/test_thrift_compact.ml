open! Core
module T = Parquet.Thrift_compact

let hex = Hex.print

let%expect_test "zigzag examples from the spec" =
  List.iter
    [ 0L; -1L; 1L; -2L; 2147483647L; -2147483648L; Int64.max_value; Int64.min_value ]
    ~f:(fun n -> printf "%Ld -> %Lu\n" n (T.zigzag64 n));
  [%expect
    {|
    0 -> 0
    -1 -> 1
    1 -> 2
    -2 -> 3
    2147483647 -> 4294967294
    -2147483648 -> 4294967295
    9223372036854775807 -> 18446744073709551614
    -9223372036854775808 -> 18446744073709551615
    |}]
;;

let%expect_test "unsigned varints" =
  List.iter [ 0L; 1L; 127L; 128L; 300L; -1L ] ~f:(fun n ->
    let buf = Buffer.create 16 in
    T.write_uvarint buf n;
    hex (Buffer.contents buf));
  [%expect
    {|
    00
    01
    7f
    80 01
    ac 02
    ff ff ff ff ff ff ff ff ff 01
    |}]
;;

let%expect_test "short field headers carry the id delta" =
  hex (T.to_string (Struct [ 1, T.i32 1; 2, T.i64 (-1); 5, T.binary "abc" ]));
  [%expect {| 15 02 16 01 38 03 61 62 63 00 |}]
;;

let%expect_test "long field headers once the delta exceeds 15" =
  hex (T.to_string (Struct [ 16, T.i32 1; 17, T.i32 2 ]));
  [%expect {| 05 20 02 15 04 00 |}]
;;

let%expect_test "booleans live in the field header" =
  hex (T.to_string (Struct [ 1, Bool true; 2, Bool false ]));
  [%expect {| 11 12 00 |}]
;;

let%expect_test "doubles are 8 little-endian bytes" =
  hex (T.to_string (Double 1.0));
  [%expect {| 00 00 00 00 00 00 f0 3f |}]
;;

let%expect_test "lists: short and long headers" =
  hex (T.to_string (List (I32, [ T.i32 1; T.i32 2; T.i32 3 ])));
  [%expect {| 35 02 04 06 |}];
  hex (T.to_string (List (Byte, List.init 15 ~f:(fun i -> T.Byte i))));
  [%expect {| f3 0f 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e |}]
;;

let%expect_test "nested structs restart the field id delta" =
  hex (T.to_string (Struct [ 1, Struct [ 3, T.i32 1 ]; 2, T.i32 2 ]));
  [%expect {| 1c 35 02 00 15 04 00 |}];
  hex (T.to_string (Struct [ 4, List (Struct, [ Struct [ 1, T.i32 0 ]; Struct [] ]) ]));
  [%expect {| 49 2c 15 00 00 00 00 |}]
;;

let%expect_test "out of order field ids are rejected" =
  print_s
    [%sexp
      (Or_error.try_with (fun () -> T.to_string (Struct [ 2, T.i32 1; 1, T.i32 2 ]))
       : string Or_error.t)];
  [%expect {| (Error ("Thrift_compact: field ids must increase" (id 1))) |}]
;;

(* [read_struct] must invert [write] for every value shape the writer produces. *)
let rec gen_value ~depth : T.t Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let scalar =
    Quickcheck.Generator.union
      [ (Bool.quickcheck_generator >>| fun b -> T.Bool b)
      ; (Int.gen_incl 0 255 >>| fun n -> T.Byte n)
      ; (Int.gen_incl (-32768) 32767 >>| fun n -> T.I16 n)
      ; (Int32.quickcheck_generator >>| fun n -> T.I32 n)
      ; (Int64.quickcheck_generator >>| fun n -> T.I64 n)
      ; (Float.gen_finite >>| fun x -> T.Double x)
      ; (String.quickcheck_generator >>| fun s -> T.Binary s)
      ]
  in
  if depth >= 3
  then scalar
  else
    Quickcheck.Generator.union
      [ scalar
      ; gen_struct ~depth:(depth + 1)
      ; (let%bind elem = gen_value ~depth:(depth + 1) in
         let ty = T.type_of elem in
         let%map rest =
           List.gen_with_length 20 (gen_value ~depth:(depth + 1))
           >>| List.filter ~f:(fun v -> T.Type.equal (T.type_of v) ty)
         in
         T.List (ty, elem :: rest))
      ]

and gen_struct ~depth =
  let open Quickcheck.Generator.Let_syntax in
  let%map fields =
    List.gen_with_length
      4
      (Quickcheck.Generator.tuple2 (Int.gen_incl 1 40) (gen_value ~depth))
  in
  let fields =
    List.dedup_and_sort fields ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  T.Struct fields
;;

let%test_unit "read_struct inverts write" =
  Quickcheck.test (gen_struct ~depth:0) ~sexp_of:T.sexp_of_t ~f:(fun v ->
    let s = T.to_string v in
    [%test_result: Sexp.t] (T.sexp_of_t (T.of_string s)) ~expect:(T.sexp_of_t v))
;;

let%expect_test "large field ids and long lists" =
  let v =
    T.Struct
      [ 1, T.List (I32, List.init 20 ~f:T.i32)
      ; 100, T.Bool false
      ; 101, T.List (Bool, [ Bool true; Bool false ])
      ]
  in
  print_s
    [%sexp
      (T.sexp_of_t (T.of_string (T.to_string v)) |> Sexp.equal (T.sexp_of_t v) : bool)];
  [%expect {| true |}]
;;

let%expect_test "malformed input is reported" =
  let show s =
    print_s [%sexp (Or_error.try_with (fun () -> T.of_string s) : T.t Or_error.t)]
  in
  show "";
  show "\x18\x05abc";
  show "\x1b\x01\x00";
  show "\x00\x00";
  [%expect
    {|
    (Error ("Thrift_compact: malformed input" (what truncated) (pos 0)))
    (Error ("Thrift_compact: malformed input" (what "binary length") (pos 2)))
    (Error
     ("Thrift_compact: malformed input" (what "unsupported type 11") (pos 1)))
    (Error ("Thrift_compact: malformed input" (what "trailing bytes") (pos 1)))
    |}]
;;
