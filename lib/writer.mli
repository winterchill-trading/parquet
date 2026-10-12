(** Streaming Parquet writer.

    Rows are buffered column by column; every [row_group_rows] rows a row group is
    written, with one PLAIN-encoded v1 data page per column chunk. [close] writes the
    footer. *)

open! Core

type 'a t

(** [create ~schema path] creates (or truncates) [path] and writes the leading magic
    bytes. [row_group_rows] defaults to 1_048_576. *)
val create : schema:'a Schema.t -> ?row_group_rows:int -> string -> 'a t

val add : 'a t -> 'a -> unit
val add_all : 'a t -> 'a list -> unit

(** Number of rows added so far. *)
val num_rows : _ t -> int

(** Flush the last row group, write the footer and close the file. *)
val close : _ t -> unit

(** [with_file ~schema path ~f] creates a writer, runs [f] and closes it, also when [f]
    raises. *)
val with_file
  :  schema:'a Schema.t
  -> ?row_group_rows:int
  -> string
  -> f:('a t -> 'b)
  -> 'b
