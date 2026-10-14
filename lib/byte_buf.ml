open! Core

(* Like [Stdlib.Buffer], the storage and its capacity sit in an immutable record, so an
   append reads them once and stores without a bounds check after its capacity check. *)
type storage =
  { bytes : Bytes.t
  ; cap : int
  }

type t =
  { mutable storage : storage
  ; mutable len : int
  }

let create n =
  let cap = Int.max n 16 in
  { storage = { bytes = Bytes.create cap; cap }; len = 0 }
;;

let length t = t.len
let clear t = t.len <- 0
let unsafe_bytes t = t.storage.bytes

external unsafe_set_int32 : Bytes.t -> int -> int32 -> unit = "%caml_bytes_set32u"
external unsafe_set_int64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external box_float : float# -> float = "%box_float"

let[@inline] int32_le x = if Sys.big_endian then swap32 x else x
let[@inline] int64_le x = if Sys.big_endian then swap64 x else x

(* Returns storage with room for [n] more bytes at [t.len]. *)
let[@inline never] grow t n =
  let cap = ref t.storage.cap in
  while t.len + n > !cap do
    cap := 2 * !cap
  done;
  let bytes = Bytes.create !cap in
  Bytes.blit ~src:t.storage.bytes ~src_pos:0 ~dst:bytes ~dst_pos:0 ~len:t.len;
  let storage = { bytes; cap = !cap } in
  t.storage <- storage;
  storage
;;

let[@inline] reserve t n =
  let storage = t.storage in
  if t.len + n > storage.cap then grow t n else storage
;;

let[@inline] add_char t c =
  let pos = t.len in
  let { bytes; cap = _ } = reserve t 1 in
  Bytes.unsafe_set bytes pos c;
  t.len <- pos + 1
;;

let[@inline] add_int32_le t n =
  let pos = t.len in
  let { bytes; cap = _ } = reserve t 4 in
  unsafe_set_int32 bytes pos (int32_le (Int32.of_int_trunc n));
  t.len <- pos + 4
;;

let[@inline] add_int64_le t n =
  let pos = t.len in
  let { bytes; cap = _ } = reserve t 8 in
  unsafe_set_int64 bytes pos (int64_le (Int64.of_int n));
  t.len <- pos + 8
;;

let[@inline] add_double_le t x =
  let pos = t.len in
  let { bytes; cap = _ } = reserve t 8 in
  unsafe_set_int64 bytes pos (int64_le (Int64.bits_of_float x));
  t.len <- pos + 8
;;

let[@inline] add_double_u_le t x =
  let pos = t.len in
  let { bytes; cap = _ } = reserve t 8 in
  unsafe_set_int64 bytes pos (int64_le (Int64.bits_of_float (box_float x)));
  t.len <- pos + 8
;;

let[@inline] add_string t s =
  let pos = t.len in
  let n = String.length s in
  let { bytes; cap = _ } = reserve t n in
  Bytes.From_string.unsafe_blit ~src:s ~src_pos:0 ~dst:bytes ~dst_pos:pos ~len:n;
  t.len <- pos + n
;;

let add_bytes t src ~pos ~len =
  if pos < 0 || len < 0 || pos + len > Bytes.length src
  then
    raise_s [%message "Parquet.Byte_buf.add_bytes: out of bounds" (pos : int) (len : int)];
  let at = t.len in
  let { bytes; cap = _ } = reserve t len in
  Bytes.blit ~src ~src_pos:pos ~dst:bytes ~dst_pos:at ~len;
  t.len <- at + len
;;

let set_int32_le t ~pos n =
  if pos < 0 || pos + 4 > t.len
  then raise_s [%message "Parquet.Byte_buf.set_int32_le: out of bounds" (pos : int)];
  unsafe_set_int32 t.storage.bytes pos (int32_le (Int32.of_int_trunc n))
;;

let contents t = Bytes.To_string.sub t.storage.bytes ~pos:0 ~len:t.len
