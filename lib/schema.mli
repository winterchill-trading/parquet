(** Typed description of the columns of a file whose rows are values of type ['a]. Each
    column has a name and an accessor that extracts its value from a row.

    All columns are [REQUIRED]: nullable and nested columns are not supported. *)

open! Core

module Column : sig
  type 'a t

  (** Parquet [INT32]. Raises at write time if a value does not fit. *)
  val int32 : string -> ('a -> int) -> 'a t

  (** Parquet [INT64]. OCaml [int] is 63 bits, so the full INT64 range is not reachable. *)
  val int64 : string -> ('a -> int) -> 'a t

  (** Parquet [DOUBLE]. *)
  val float : string -> ('a -> float) -> 'a t

  (** [BYTE_ARRAY] with the [STRING] logical type. The value must be UTF-8. *)
  val utf8 : string -> ('a -> string) -> 'a t

  (** Raw [BYTE_ARRAY]. *)
  val bytes : string -> ('a -> string) -> 'a t

  (** [INT64] nanoseconds since the epoch with logical type
      [TIMESTAMP(NANOS, isAdjustedToUTC)].

      [adjusted_to_utc] defaults to [false]. DuckDB reads [isAdjustedToUTC=true] as
      [TIMESTAMP WITH TIME ZONE], which has microsecond resolution, and silently drops the
      nanoseconds; with [false] it reads [TIMESTAMP_NS] and keeps them. The stored
      integers are the same either way. *)
  val timestamp_ns : ?adjusted_to_utc:bool -> string -> ('a -> Time_ns.t) -> 'a t

  (** [INT64] with logical type [DECIMAL(precision, scale)]. The accessor returns the
      unscaled value: [12.34] at scale 2 is [1234]. Requires [1 <= precision <= 18] and
      [0 <= scale <= precision]. *)
  val decimal : precision:int -> scale:int -> string -> ('a -> int) -> 'a t

  (** Parquet [BOOLEAN], bit-packed. *)
  val bool : string -> ('a -> bool) -> 'a t

  val name : _ t -> string

  (** How the writer reads a value of this column. *)
  module Accessor : sig
    type 'a t =
      | Int32 of ('a -> int)
      | Int64 of ('a -> int)
      | Double of ('a -> float)
      | Byte_array of ('a -> string)
      | Boolean of ('a -> bool)
  end

  val accessor : 'a t -> 'a Accessor.t
  val physical_type : _ t -> Metadata.Physical_type.t
  val schema_element : _ t -> Metadata.Schema_element.t
end

type 'a t

(** Raises on an empty column list or duplicate column names. *)
val create : 'a Column.t list -> 'a t

val columns : 'a t -> 'a Column.t list

(** The flattened schema written to the footer: a root element followed by one element per
    column. *)
val schema_elements : _ t -> Metadata.Schema_element.t list
