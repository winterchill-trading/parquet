open! Core

type t =
  { mutable bytes : Bytes.t
  ; mutable len : int
  }

let create n = { bytes = Bytes.create (Int.max n 16); len = 0 }
let length t = t.len
let clear t = t.len <- 0
let unsafe_bytes t = t.bytes

let[@inline never] grow t need =
  let cap = ref (Bytes.length t.bytes) in
  while !cap < t.len + need do
    cap := 2 * !cap
  done;
  let bytes = Bytes.create !cap in
  Bytes.blit ~src:t.bytes ~src_pos:0 ~dst:bytes ~dst_pos:0 ~len:t.len;
  t.bytes <- bytes
;;

let[@inline] reserve t n = if t.len + n > Bytes.length t.bytes then grow t n

let add_char t c =
  reserve t 1;
  Bytes.unsafe_set t.bytes t.len c;
  t.len <- t.len + 1
;;

let add_int32_le t n =
  reserve t 4;
  Stdlib.Bytes.set_int32_le t.bytes t.len (Int32.of_int_trunc n);
  t.len <- t.len + 4
;;

let add_int64_le t n =
  reserve t 8;
  Stdlib.Bytes.set_int64_le t.bytes t.len (Int64.of_int n);
  t.len <- t.len + 8
;;

let add_double_le t x =
  reserve t 8;
  Stdlib.Bytes.set_int64_le t.bytes t.len (Int64.bits_of_float x);
  t.len <- t.len + 8
;;

external box_float : float# -> float = "%box_float"

let add_double_u_le t x =
  reserve t 8;
  Stdlib.Bytes.set_int64_le t.bytes t.len (Int64.bits_of_float (box_float x));
  t.len <- t.len + 8
;;

let add_string t s =
  let n = String.length s in
  reserve t n;
  Bytes.From_string.unsafe_blit ~src:s ~src_pos:0 ~dst:t.bytes ~dst_pos:t.len ~len:n;
  t.len <- t.len + n
;;

let contents t = Bytes.To_string.sub t.bytes ~pos:0 ~len:t.len
