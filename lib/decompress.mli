(** Decompressors for the Parquet codecs that have no OCaml binding in this library's
    dependencies, written from their format descriptions. Only decompression: the writer
    compresses with zstd.

    Both raise on malformed input (a copy before the start of the output, a length past
    the end of the input or of the declared size); they never read or write out of bounds. *)

open! Core

(** Raw Snappy block format (as Parquet uses it, without the framing format): a varint
    uncompressed length, then literals and back-references.

    @raise on malformed input. *)
val snappy : string -> string

(** LZ4 block format ([LZ4_RAW] in Parquet), which does not record the decompressed size;
    [uncompressed_size] comes from the page header.

    @raise on malformed input or if the output is not exactly [uncompressed_size] bytes. *)
val lz4_raw : string -> uncompressed_size:int -> string
