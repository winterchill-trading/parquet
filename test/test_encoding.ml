open! Core
module E = Parquet.Encoding

let encode f =
  let buf = Buffer.create 16 in
  f buf;
  Hex.print (Buffer.contents buf)
;;

let%expect_test "numbers are little-endian" =
  encode (fun buf -> E.int32 buf 1);
  encode (fun buf -> E.int32 buf (-2));
  encode (fun buf -> E.int64 buf 0x0102030405060708);
  encode (fun buf -> E.float32 buf 1.0);
  encode (fun buf -> E.double buf (-2.5));
  [%expect
    {|
    01 00 00 00
    fe ff ff ff
    08 07 06 05 04 03 02 01
    00 00 80 3f
    00 00 00 00 00 00 04 c0
    |}]
;;

let%expect_test "int32 range is checked" =
  print_s
    [%sexp
      (Or_error.try_with (fun () -> encode (fun buf -> E.int32 buf (1 lsl 31)))
       : unit Or_error.t)];
  [%expect {| (Error ("Parquet.Encoding.int32: out of range" (n 2147483648))) |}]
;;

let%expect_test "byte arrays" =
  encode (fun buf -> E.byte_array buf "hi");
  encode (fun buf -> E.byte_array buf "");
  encode (fun buf -> E.fixed_len_byte_array buf ~len:3 "abc");
  [%expect {|
    02 00 00 00 68 69
    00 00 00 00
    61 62 63
    |}]
;;

let%expect_test "booleans are bit-packed lsb first" =
  encode (fun buf ->
    let b = E.Booleans.create () in
    List.iter
      [ true; false; true; true; false; false; false; false; true; true ]
      ~f:(E.Booleans.add b buf);
    E.Booleans.flush b buf);
  [%expect {| 0d 03 |}]
;;
