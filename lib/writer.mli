(** Streaming Parquet writer.

    Rows are buffered column by column; every [row_group_rows] rows a row group is
    written, with one v1 data page per column chunk (PLAIN, or PLAIN_DICTIONARY after a
    dictionary page for {!Schema.Column.dictionary} columns; definition levels for
    {!Schema.Column.optional} ones). [close] writes the footer.

    {[
      Parquet.Writer.with_file ~schema ~compression:`Zstd "trades.parquet" ~f:(fun w ->
        List.iter trades ~f:(Parquet.Writer.add w))
    ]}

    A whole row group is held in memory until it is written; lower [row_group_rows] to
    bound memory. *)

open! Core

type 'a t

(** [create ~schema path] creates (or truncates) [path] and writes the leading magic
    bytes. [row_group_rows] defaults to 1_048_576. [compression] defaults to [`None];
    [`Zstd] compresses each page at zstd level 3. [key_value_metadata] (default none) is
    written to the footer as application-defined key-value pairs.

    @raise if [row_group_rows < 1] or the file cannot be created. *)
val create
  :  schema:'a Schema.t
  -> ?row_group_rows:int
  -> ?compression:[ `None | `Zstd ]
  -> ?key_value_metadata:(string * string) list
  -> string
  -> 'a t

(** Append one row; writes a row group when [row_group_rows] rows are buffered.

    @raise if the writer is closed, or a value does not fit its column ([int32] range). *)
val add : 'a t -> 'a -> unit

(** [List.iter rows ~f:(add t)]. *)
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
  (** A column of the writer, looked up by name, of the OCaml type its values have. *)
  type int_column

  type float_column
  type string_column
  type bool_column

  (** The column with this name. Raises if there is none or it has another type. [int]
      covers [int32], [int64], [decimal] (the unscaled value) and [timestamp_ns]
      (nanoseconds since the epoch) columns; [string] covers [utf8] and [bytes]. Optional
      columns are found by the type of their values. *)
  val int : _ t -> string -> int_column

  val float : _ t -> string -> float_column
  val string : _ t -> string -> string_column
  val bool : _ t -> string -> bool_column

  (** Raises if an [int32] column gets a value that does not fit in 32 bits. *)
  val add_int : int_column -> int -> unit

  (** Boxes [float] at the call unless the call is inlined; [add_float_u] never does. *)
  val add_float : float_column -> float -> unit

  (** {!add_float} for an unboxed float. *)
  val add_float_u : float_column -> float# -> unit

  (** For [utf8] columns the string must be valid UTF-8; it is not checked. *)
  val add_string : string_column -> string -> unit

  val add_bool : bool_column -> bool -> unit

  (** Give the row a null in an {!Schema.Column.optional} column.

      @raise if the column is not optional. *)
  val add_null_int : int_column -> unit

  (** See {!add_null_int}. *)
  val add_null_float : float_column -> unit

  (** See {!add_null_int}. *)
  val add_null_string : string_column -> unit

  (** See {!add_null_int}. *)
  val add_null_bool : bool_column -> unit

  (** Ends the row. Raises if a column did not get exactly one value since the previous
      row; the writer is then unusable. *)
  val end_row : _ t -> unit
end

(** Number of rows added so far. *)
val num_rows : _ t -> int

(** Flush the last row group, write the footer and close the file. Calling it again does
    nothing. Without [close] the file has no footer and no reader accepts it. *)
val close : _ t -> unit

(** [with_file ~schema path ~f] creates a writer, runs [f] and closes it, also when [f]
    raises. *)
val with_file
  :  schema:'a Schema.t
  -> ?row_group_rows:int
  -> ?compression:[ `None | `Zstd ]
  -> ?key_value_metadata:(string * string) list
  -> string
  -> f:('a t -> 'b)
  -> 'b
