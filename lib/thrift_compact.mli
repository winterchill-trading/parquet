(** Writer for the Thrift compact protocol, the encoding Parquet uses for its file
    metadata and page headers.

    A message is described as a small tree of {!t} values and serialized in one pass.
    Field ids inside a struct are delta-encoded automatically; give fields in increasing
    id order, as generated Thrift code does. *)

open! Core

(** Wire type of a value, used for list element headers. *)
module Type : sig
  type t =
    | Bool
    | Byte
    | I16
    | I32
    | I64
    | Double
    | Binary
    | List
    | Struct
  [@@deriving sexp_of, equal]
end

type t =
  | Bool of bool
  | Byte of int
  | I16 of int
  | I32 of int32
  | I64 of int64
  | Double of float
  | Binary of string
  | List of Type.t * t list
  | Struct of (int * t) list
[@@deriving sexp_of]

val type_of : t -> Type.t

(** Convenience constructors. [i32] raises if the value does not fit in 32 bits. *)
val i32 : int -> t

val i64 : int -> t
val binary : string -> t

(** Zigzag mapping of signed to unsigned integers: 0, -1, 1, -2 map to 0, 1, 2,

    3. *)
val zigzag64 : int64 -> int64

(** Append an unsigned LEB128 varint (the argument is read as unsigned). *)
val write_uvarint : Buffer.t -> int64 -> unit

(** Append the encoding of a value. A [Struct] is terminated by its stop byte. *)
val write : Buffer.t -> t -> unit

val to_string : t -> string

(** {2 Reading} *)

(** [read_struct s ~pos] reads the struct that starts at [pos] in [s] and returns it with
    the position just after its stop byte. Field ids are absolute in the result. Sets are
    read as lists; maps are not supported (Parquet's metadata has none).

    @raise if the input is truncated or malformed, or nests deeper than 64 levels. *)
val read_struct : string -> pos:int -> t * int

(** [of_string s] reads one struct that must span all of [s].

    @raise like {!read_struct}, and if bytes are left over. *)
val of_string : string -> t
