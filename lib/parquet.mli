(** Apache Parquet files from OCaml: a streaming writer and a reader for flat schemas.

    Describe the columns of a row type with {!Schema}, stream rows into a file with
    {!Writer}, read files back with {!Reader}:

    {[
      open! Core

      type trade =
        { ts : Time_ns.t
        ; coin : string
        ; px : int (* unscaled, 6 decimals *)
        ; fee : int option
        }

      let schema =
        Parquet.Schema.create
          Parquet.Schema.Column.
            [ timestamp_ns "ts" (fun t -> t.ts)
            ; dictionary (utf8 "coin" (fun t -> t.coin))
            ; decimal ~precision:18 ~scale:6 "px" (fun t -> t.px)
            ; optional (decimal ~precision:18 ~scale:6) "fee" (fun t -> t.fee)
            ]
      ;;

      let write trades =
        Parquet.Writer.with_file ~schema ~compression:`Zstd "trades.parquet" ~f:(fun w ->
          List.iter trades ~f:(Parquet.Writer.add w))
      ;;

      let count_rows () =
        Parquet.Reader.with_file "trades.parquet" ~f:(fun r ->
          Parquet.Reader.fold_rows r ~init:0 ~f:(fun n _row -> n + 1))
      ;;
    ]}

    [examples/trades.ml] in the repository is the same program, built and run in CI.

    The lower-level modules ({!Metadata}, {!Thrift_compact}, {!Encoding}, {!Rle_hybrid},
    {!Byte_buf}, {!Zstd_bytes}, {!Decompress}, {!Delta}) are the building blocks of the
    two above, exposed for tools that need the file structures themselves. *)

(** Typed column descriptions. *)
module Schema = Schema

(** Streaming writer. *)
module Writer = Writer

(** Reader for flat files. *)
module Reader = Reader

(** Footer and page header structs ([parquet.thrift]). *)
module Metadata = Metadata

(** Thrift compact protocol. *)
module Thrift_compact = Thrift_compact

(** PLAIN encoders. *)
module Encoding = Encoding

(** RLE / bit-packing hybrid encoding. *)
module Rle_hybrid = Rle_hybrid

(** Growable byte buffer. *)
module Byte_buf = Byte_buf

(** zstd compression over [Bytes.t]. *)
module Zstd_bytes = Zstd_bytes

(** Snappy and LZ4 decompression for the reader. *)
module Decompress = Decompress

(** DELTA and BYTE_STREAM_SPLIT decoders for the reader. *)
module Delta = Delta
