(** Reader for Parquet files with a flat schema: every column a leaf under the root,
    [REQUIRED] or [OPTIONAL]. Covers what {!Writer} writes and the common subset other
    writers produce (DuckDB, Arrow with default settings):

    - v1 data pages and dictionary pages, several pages per column chunk;
    - PLAIN, PLAIN_DICTIONARY / RLE_DICTIONARY and RLE (booleans) encodings;
    - definition levels (RLE/bit-packed hybrid);
    - uncompressed and zstd pages;
    - all physical types but the deprecated INT96.

    Not supported, reported as errors: nested or repeated columns, data page v2, DELTA and
    BYTE_STREAM_SPLIT encodings, codecs other than zstd, encrypted files.

    Values come back untyped ({!Value.t}), one array per column chunk; the logical type
    (timestamp unit, decimal scale, UTF-8) is in {!columns}.

    {[
      let%bind.Or_error rows =
        Parquet.Reader.with_file "trades.parquet" ~f:(fun r ->
          Parquet.Reader.fold_rows r ~init:[] ~f:(fun acc row -> row :: acc))
      in
      ...
    ]}

    A column chunk is read into memory whole; the reader does not use statistics or page
    indexes to skip data. *)

open! Core

module Value : sig
  (** [Int] holds INT32 and INT64 values (an INT64 outside the 63-bit [int] range is an
      error), so decimals are unscaled and timestamps are counts of their unit. [Float]
      holds FLOAT and DOUBLE. [String] holds BYTE_ARRAY and FIXED_LEN_BYTE_ARRAY. *)
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
  [@@deriving sexp_of, compare, equal]
end

type t

(** Opens the file and reads its footer.

    Errors if the file is not Parquet, the footer is malformed, or the schema is not flat. *)
val create : string -> t Or_error.t

val close : t -> unit

(** [with_file path ~f] opens [path], runs [f] and closes the file, also if [f] raises. *)
val with_file : string -> f:(t -> 'a Or_error.t) -> 'a Or_error.t

val metadata : t -> Metadata.File_meta_data.t

(** The leaf schema elements, in column order. *)
val columns : t -> Metadata.Schema_element.t list

val num_rows : t -> int
val num_row_groups : t -> int

(** The values of the column [name] in row group [row_group], one per row, [Null] where
    the value is absent. *)
val read_column : t -> row_group:int -> string -> Value.t array Or_error.t

(** The rows of a row group, each with one value per column in {!columns} order. *)
val read_row_group : t -> int -> Value.t array array Or_error.t

(** Folds over every row of the file, row group by row group. *)
val fold_rows : t -> init:'acc -> f:('acc -> Value.t array -> 'acc) -> 'acc Or_error.t
