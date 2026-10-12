(** The subset of the Parquet file metadata ([parquet.thrift], vendored under [docs/])
    that this writer produces, with Thrift compact serialization.

    Field ids follow [parquet.thrift]; each [to_thrift] lists only the fields that are
    set. *)

open! Core

module Physical_type : sig
  type t =
    | Boolean
    | Int32
    | Int64
    | Float
    | Double
    | Byte_array
    | Fixed_len_byte_array
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end

module Converted_type : sig
  type t =
    | Utf8
    | Decimal
    | Timestamp_millis
    | Timestamp_micros
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end

module Repetition : sig
  type t =
    | Required
    | Optional
    | Repeated
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end

module Encoding : sig
  type t =
    | Plain
    | Rle
    | Bit_packed
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end

module Compression_codec : sig
  type t =
    | Uncompressed
    | Zstd
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end

module Page_type : sig
  type t = Data_page [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end
