open! Core
open Parquet

let encode ~bit_width values =
  let buf = Byte_buf.create 16 in
  Rle_hybrid.encode ~bit_width ~len:(Array.length values) ~get:(Array.get values) buf;
  Byte_buf.contents buf
;;

let%expect_test "spec example and runs" =
  (* The bit-packing example from the Parquet encodings page: 0..7 at bit width 3. *)
  Hex.print (encode ~bit_width:3 [| 0; 1; 2; 3; 4; 5; 6; 7 |]);
  (* Ten 1s: one RLE run, header 10 lsl 1. *)
  Hex.print (encode ~bit_width:1 (Array.create ~len:10 1));
  (* A short mixed tail is padded to a whole group. *)
  Hex.print (encode ~bit_width:1 [| 1; 0; 1 |]);
  (* Nine-bit values take two bytes in an RLE run. *)
  Hex.print (encode ~bit_width:9 (Array.create ~len:8 300));
  [%expect {|
    03 88 c6 fa
    14 01
    03 05
    10 2c 01
    |}]
;;

let%expect_test "values that do not fit are rejected" =
  print_s
    [%sexp
      (Or_error.try_with (fun () -> encode ~bit_width:2 [| 4 |]) : string Or_error.t)];
  [%expect
    {| (Error ("Parquet.Rle_hybrid.encode: value does not fit" (v 4) (bit_width 2))) |}]
;;

let%expect_test "bit_width" =
  List.iter [ 0; 1; 2; 3; 255; 256 ] ~f:(fun max_value ->
    printf "%d -> %d\n" max_value (Rle_hybrid.bit_width ~max_value));
  [%expect
    {|
    0 -> 1
    1 -> 1
    2 -> 2
    3 -> 2
    255 -> 8
    256 -> 9
    |}]
;;

let%test_unit "decode inverts encode" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind bit_width = Int.gen_incl 1 32 in
    let max = (1 lsl bit_width) - 1 in
    (* Few distinct values, so that runs happen as well as bit-packed stretches. *)
    let%bind palette = List.gen_with_length 3 (Int.gen_incl 0 max) in
    let%map values = List.gen_non_empty (Quickcheck.Generator.of_list palette) in
    bit_width, Array.of_list values
  in
  Quickcheck.test gen ~sexp_of:[%sexp_of: int * int array] ~f:(fun (bit_width, values) ->
    let s = encode ~bit_width values in
    let decoded, stop =
      Rle_hybrid.decode
        ~bit_width
        s
        ~pos:0
        ~stop:(String.length s)
        ~len:(Array.length values)
    in
    [%test_result: int array] decoded ~expect:values;
    [%test_result: int] stop ~expect:(String.length s))
;;

let%test_unit "long runs round trip" =
  Quickcheck.test
    (List.gen_non_empty
       (Quickcheck.Generator.tuple2 Bool.quickcheck_generator (Int.gen_incl 1 50)))
    ~sexp_of:[%sexp_of: (bool * int) list]
    ~f:(fun runs ->
      let values =
        Array.concat_map (Array.of_list runs) ~f:(fun (b, n) ->
          Array.create ~len:n (Bool.to_int b))
      in
      let s = encode ~bit_width:1 values in
      let decoded, _ =
        Rle_hybrid.decode
          ~bit_width:1
          s
          ~pos:0
          ~stop:(String.length s)
          ~len:(Array.length values)
      in
      [%test_result: int array] decoded ~expect:values)
;;

let%expect_test "truncated input is reported" =
  let s = encode ~bit_width:3 [| 0; 1; 2; 3; 4; 5; 6; 7 |] in
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         Rle_hybrid.decode ~bit_width:3 s ~pos:0 ~stop:2 ~len:8)
       : (int array * int) Or_error.t)];
  [%expect
    {|
    (Error
     ("Parquet.Rle_hybrid.decode: malformed stream"
      (what "truncated bit-packed run") (pos 1)))
    |}]
;;
