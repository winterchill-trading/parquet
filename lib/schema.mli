(** Typed description of the columns of a file whose rows are values of type ['a]. Each
    column has a name and an accessor that extracts its value from a row.

    {[
      type trade =
        { ts : Time_ns.t
        ; coin : string
        ; px : int option
        }

      let schema =
        Parquet.Schema.create
          Parquet.Schema.Column.
            [ timestamp_ns "ts" (fun t -> t.ts)
            ; dictionary (utf8 "coin" (fun t -> t.coin))
            ; optional (decimal ~precision:18 ~scale:6) "px" (fun t -> t.px)
            ]
      ;;
    ]}

    Columns are [REQUIRED] unless built with {!Column.optional}. Nested and repeated
    columns are not supported. *)

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

  (** [optional make name f] is the [OPTIONAL] (nullable) column [make name], whose value
      for a row is [f row] and null when that is [None]. [make] is one of the combinators
      above, partially applied if it takes parameters:

      {[
        optional
          int64
          "qty"
          (fun r -> r.qty)
          optional
          (decimal ~precision:18 ~scale:6)
          "px"
          (fun r -> r.px)
          optional
          timestamp_ns
          "closed_at"
          (fun r -> r.closed_at)
      ]}

      Nulls are written as definition levels (RLE/bit-packed hybrid, bit width 1) and
      counted in the [null_count] statistic; only present values are encoded.

      @raise if [make] already builds an optional column. *)
  val optional : (string -> ('b -> 'b) -> 'b t) -> string -> ('a -> 'b option) -> 'a t

  (** Dictionary-encode the column: each column chunk gets a dictionary page with the
      distinct values (PLAIN) and the data page holds their indices (RLE/bit-packed
      hybrid). Worth it for low-cardinality columns (symbols, venues, enums). If a chunk's
      dictionary grows past 1 MiB the chunk falls back to PLAIN. The writer keeps a hash
      table per chunk, so rows cost a few words of allocation more than PLAIN columns.

      @raise on a boolean column. *)
  val dictionary : 'a t -> 'a t

  val name : _ t -> string
  val is_optional : _ t -> bool
  val is_dictionary : _ t -> bool

  (** How the writer reads a value of this column. *)
  module Accessor : sig
    type 'a t =
      | Int32 of ('a -> int)
      | Int64 of ('a -> int)
      | Double of ('a -> float)
      | Byte_array of ('a -> string)
      | Boolean of ('a -> bool)
  end

  (** For an optional column, the accessor raises on rows whose value is [None]; use
      {!kind} to tell the two apart. *)
  val accessor : 'a t -> 'a Accessor.t

  (** How the writer reads a row: the value, or for an optional column the option and the
      accessor of the present value. *)
  module Kind : sig
    type 'a t =
      | Required of 'a Accessor.t
      | Optional : ('a -> 'b option) * 'b Accessor.t -> 'a t
  end

  val kind : 'a t -> 'a Kind.t
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
