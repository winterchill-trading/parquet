(** Streaming Parquet writer.

    Rows are buffered column by column; every [row_group_rows] rows a row group is
    written, with one PLAIN-encoded v1 data page per column chunk. [close] writes the
    footer. *)

open! Core

type 'a t

(** [create ~schema path] creates (or truncates) [path] and writes the leading magic
    bytes. [row_group_rows] defaults to 1_048_576. [compression] defaults to [`None];
    [`Zstd] compresses each page at zstd level 3. *)
val create
  :  schema:'a Schema.t
  -> ?row_group_rows:int
  -> ?compression:[ `None | `Zstd ]
  -> string
  -> 'a t

val add : 'a t -> 'a -> unit
val add_all : 'a t -> 'a list -> unit

(** Rows appended column by column, without the schema accessors: no closure call per cell
    and no boxed value on the way. Look the columns up once, then for each row give every
    column exactly one value, in any order, and call [end_row]. Rows written this way and
    with {!add} can be mixed, and the file is the same as the one {!add} writes for the
    same values.

    {[
      let ts = Writer.Direct.int w "ts"
      and sz = Writer.Direct.float w "sz" in
      Writer.Direct.add_int ts (Time_ns.to_int_ns_since_epoch trade.ts);
      Writer.Direct.add_float sz trade.sz;
      Writer.Direct.end_row w
    ]} *)
module Direct : sig
  type int_column
  type float_column
  type string_column
  type bool_column

  (** The column with this name. Raises if there is none or it has another type. [int]
      covers [int32], [int64], [decimal] (the unscaled value) and [timestamp_ns]
      (nanoseconds since the epoch) columns; [string] covers [utf8] and [bytes]. *)
  val int : _ t -> string -> int_column

  val float : _ t -> string -> float_column
  val string : _ t -> string -> string_column
  val bool : _ t -> string -> bool_column

  (** Raises if an [int32] column gets a value that does not fit in 32 bits. *)
  val add_int : int_column -> int -> unit

  (** Boxes [float] at the call unless the call is inlined; [add_float_u] never does. *)
  val add_float : float_column -> float -> unit

  val add_float_u : float_column -> float# -> unit
  val add_string : string_column -> string -> unit
  val add_bool : bool_column -> bool -> unit

  (** Ends the row. Raises if a column did not get exactly one value since the previous
      row; the writer is then unusable. *)
  val end_row : _ t -> unit
end

(** Number of rows added so far. *)
val num_rows : _ t -> int

(** Flush the last row group, write the footer and close the file. *)
val close : _ t -> unit

(** [with_file ~schema path ~f] creates a writer, runs [f] and closes it, also when [f]
    raises. *)
val with_file
  :  schema:'a Schema.t
  -> ?row_group_rows:int
  -> ?compression:[ `None | `Zstd ]
  -> string
  -> f:('a t -> 'b)
  -> 'b
