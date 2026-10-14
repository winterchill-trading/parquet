(** Growable byte buffer whose storage is reachable without a copy, for column data that
    is compressed or written straight from its bytes. Numbers are little-endian. *)

open! Core

type t

(** [create n] is an empty buffer with room for [n] bytes (at least 16). It grows by
    doubling. *)
val create : int -> t

(** Number of bytes appended since creation or the last {!clear}. *)
val length : t -> int

(** Reset the length to zero and keep the storage. *)
val clear : t -> unit

(** The underlying storage; only the first [length t] bytes are meaningful. Valid until
    the next append. *)
val unsafe_bytes : t -> Bytes.t

val add_char : t -> char -> unit

(** The low 32 bits of the value, little-endian; the range is not checked. *)
val add_int32_le : t -> int -> unit

(** The value sign-extended to 64 bits, little-endian. *)
val add_int64_le : t -> int -> unit

(** IEEE 754 double, little-endian. *)
val add_double_le : t -> float -> unit

(** [add_double_le] for an unboxed float. *)
val add_double_u_le : t -> float# -> unit

(** The bytes of the string, with no length prefix. *)
val add_string : t -> string -> unit
(** Append [len] bytes of [src] from [pos].

    @raise if the range is outside [src]. *)
val add_bytes : t -> Bytes.t -> pos:int -> len:int -> unit

(** Overwrite the 4 bytes at [pos] (already appended) with [n], for a length prefix
    written before its contents are known.

    @raise if [pos + 4 > length t]. *)
val set_int32_le : t -> pos:int -> int -> unit

(** A copy of the first [length t] bytes. *)
val contents : t -> string
