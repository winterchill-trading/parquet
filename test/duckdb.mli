(** Helpers that check written files with the DuckDB CLI, which must be on [PATH]. *)

open! Core

(** [with_parquet_file f] calls [f] with a fresh path in a temporary directory and removes
    the directory afterwards. *)
val with_parquet_file : (string -> 'a) -> 'a

(** Run [sql] with [duckdb -csv] and print its output (stdout and stderr). Every [$file]
    in [sql] is replaced by the quoted [file]. *)
val query : file:string -> string -> unit
