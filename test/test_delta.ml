open! Core
open Parquet

let show_ints s =
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Delta.binary_packed s ~pos:0 ~stop:(String.length s))
       : (int64 array * int) Or_error.t)]
;;

let%expect_test "DELTA_BINARY_PACKED examples" =
  (* 1, 2, 3, 4, 5 from the Parquet encodings page: block 128, 4 miniblocks, 5 values,
     first 1, min delta 1, every bit width 0. *)
  show_ints "\x80\x01\x04\x05\x02\x02\x00\x00\x00\x00";
  (* 7, 5, 3, 1, 2, 3, 4, 5: min delta -2, deltas 0 0 0 3 3 3 3 at bit width 2. *)
  show_ints
    ("\x80\x01\x04\x08\x0e\x03\x02\x00\x00\x00" ^ "\xc0\xff\x00\x00\x00\x00\x00\x00");
  (* A count that the remaining bytes cannot hold. *)
  show_ints "\x80\x01\x04\xff\xff\x03\x00";
  [%expect
    {|
    (Ok ((1 2 3 4 5) 10))
    (Ok ((7 5 3 1 2 3 4 5) 18))
    (Error ("Parquet.Delta: malformed input" (what "value count") (pos 7)))
    |}]
;;

let%expect_test "BYTE_STREAM_SPLIT" =
  print_endline (Delta.byte_stream_split "aAbBcC" ~pos:0 ~n:2 ~width:3);
  [%expect {| abcABC |}]
;;
