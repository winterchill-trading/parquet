(** Growable byte buffer whose storage is reachable without a copy, for column data that
    is compressed or written straight from its bytes. Numbers are little-endian. *)

open! Core

type t

val create : int -> t
val length : t -> int

(** Reset the length to zero and keep the storage. *)
val clear : t -> unit

(** The underlying storage; only the first [length t] bytes are meaningful. Valid until the
    next append. *)
val unsafe_bytes : t -> Bytes.t

val add_char : t -> char -> unit
val add_int32_le : t -> int -> unit
val add_int64_le : t -> int -> unit
val add_double_le : t -> float -> unit

(** [add_double_le] for an unboxed float. *)
val add_double_u_le : t -> float# -> unit

val add_string : t -> string -> unit
val contents : t -> string
