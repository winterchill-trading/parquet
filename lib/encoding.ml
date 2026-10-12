open! Core

let int32 buf n =
  if n < -0x8000_0000 || n > 0x7FFF_FFFF
  then raise_s [%message "Parquet.Encoding.int32: out of range" (n : int)];
  Stdlib.Buffer.add_int32_le buf (Int32.of_int_trunc n)
;;

let int64 buf n = Stdlib.Buffer.add_int64_le buf (Int64.of_int n)
let float32 buf x = Stdlib.Buffer.add_int32_le buf (Int32.bits_of_float x)
let double buf x = Stdlib.Buffer.add_int64_le buf (Int64.bits_of_float x)

let byte_array buf s =
  Stdlib.Buffer.add_int32_le buf (Int32.of_int_exn (String.length s));
  Buffer.add_string buf s
;;

let fixed_len_byte_array buf ~len s =
  if String.length s <> len
  then
    raise_s
      [%message
        "Parquet.Encoding.fixed_len_byte_array: wrong length" (len : int) (s : string)];
  Buffer.add_string buf s
;;

module Booleans = struct
  type t =
    { mutable byte : int
    ; mutable bits : int
    }

  let create () = { byte = 0; bits = 0 }

  let add t buf b =
    if b then t.byte <- t.byte lor (1 lsl t.bits);
    t.bits <- t.bits + 1;
    if t.bits = 8
    then (
      Buffer.add_char buf (Char.unsafe_of_int t.byte);
      t.byte <- 0;
      t.bits <- 0)
  ;;

  let flush t buf =
    if t.bits > 0 then Buffer.add_char buf (Char.unsafe_of_int t.byte);
    t.byte <- 0;
    t.bits <- 0
  ;;
end
