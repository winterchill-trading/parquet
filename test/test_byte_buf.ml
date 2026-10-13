open! Core
open Parquet

(* [Byte_buf] appends must produce what [Encoding] writes into a [Buffer.t]. *)

let%test_unit "Byte_buf matches Buffer for every append" =
  Quickcheck.test
    (List.quickcheck_generator
       (Quickcheck.Generator.variant4
          (Int.gen_incl (-0x8000_0000) 0x7FFF_FFFF)
          Int.quickcheck_generator
          Float.quickcheck_generator
          String.quickcheck_generator))
    ~f:(fun ops ->
      let t = Byte_buf.create 1 in
      let b = Buffer.create 1 in
      List.iter ops ~f:(function
        | `A n ->
          Byte_buf.add_int32_le t n;
          Encoding.int32 b n
        | `B n ->
          Byte_buf.add_int64_le t n;
          Encoding.int64 b n
        | `C x ->
          Byte_buf.add_double_le t x;
          Encoding.double b x
        | `D s ->
          Byte_buf.add_string t s;
          Byte_buf.add_char t 'x';
          Buffer.add_string b s;
          Buffer.add_char b 'x');
      [%test_result: string] (Byte_buf.contents t) ~expect:(Buffer.contents b);
      [%test_result: int] (Byte_buf.length t) ~expect:(Buffer.length b))
;;

let%expect_test "clear keeps the storage" =
  let t = Byte_buf.create 16 in
  Byte_buf.add_string t (String.make 100 'a');
  let bytes = Byte_buf.unsafe_bytes t in
  Byte_buf.clear t;
  Byte_buf.add_int64_le t 1;
  print_s
    [%sexp
      (phys_equal bytes (Byte_buf.unsafe_bytes t) : bool), (Byte_buf.contents t : string)];
  [%expect {| (true "\001\000\000\000\000\000\000\000") |}]
;;
