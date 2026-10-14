(** PLAIN encoding of required (non-null) values, appended to a buffer.

    All numbers are little-endian. Byte arrays carry a 4-byte length prefix; fixed-length
    byte arrays do not. *)

open! Core

(** Raises if the value does not fit in 32 bits. *)
val int32 : Buffer.t -> int -> unit

(** The value sign-extended to 64 bits. *)
val int64 : Buffer.t -> int -> unit

(** IEEE 754 single precision (Parquet [FLOAT]); the value is rounded. *)
val float32 : Buffer.t -> float -> unit

(** IEEE 754 double precision (Parquet [DOUBLE]). *)
val double : Buffer.t -> float -> unit

(** 4-byte length, then the bytes. *)
val byte_array : Buffer.t -> string -> unit

(** Raises if [String.length s <> len]. *)
val fixed_len_byte_array : Buffer.t -> len:int -> string -> unit

(** Booleans are bit-packed, least significant bit first, the last byte padded with zeros. *)
module Booleans : sig
  (** The pending bits of a partial byte. *)
  type t

  val create : unit -> t

  (** Append a bit; a byte is written to the buffer every 8 bits. *)
  val add : t -> Buffer.t -> bool -> unit

  (** Write the partial last byte, if any, and reset. *)
  val flush : t -> Buffer.t -> unit
end
