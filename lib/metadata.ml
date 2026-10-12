open! Core

let enum n = Thrift_compact.i32 n

module Physical_type = struct
  type t =
    | Boolean
    | Int32
    | Int64
    | Float
    | Double
    | Byte_array
    | Fixed_len_byte_array
  [@@deriving sexp_of, equal]

  let to_thrift t =
    enum
      (match t with
       | Boolean -> 0
       | Int32 -> 1
       | Int64 -> 2
       | Float -> 4
       | Double -> 5
       | Byte_array -> 6
       | Fixed_len_byte_array -> 7)
  ;;
end

module Converted_type = struct
  type t =
    | Utf8
    | Decimal
    | Timestamp_millis
    | Timestamp_micros
  [@@deriving sexp_of, equal]

  let to_thrift t =
    enum
      (match t with
       | Utf8 -> 0
       | Decimal -> 5
       | Timestamp_millis -> 9
       | Timestamp_micros -> 10)
  ;;
end

module Repetition = struct
  type t =
    | Required
    | Optional
    | Repeated
  [@@deriving sexp_of, equal]

  let to_thrift t =
    enum
      (match t with
       | Required -> 0
       | Optional -> 1
       | Repeated -> 2)
  ;;
end

module Encoding = struct
  type t =
    | Plain
    | Rle
    | Bit_packed
  [@@deriving sexp_of, equal]

  let to_thrift t =
    enum
      (match t with
       | Plain -> 0
       | Rle -> 3
       | Bit_packed -> 4)
  ;;
end

module Compression_codec = struct
  type t =
    | Uncompressed
    | Zstd
  [@@deriving sexp_of, equal]

  let to_thrift t =
    enum
      (match t with
       | Uncompressed -> 0
       | Zstd -> 6)
  ;;
end

module Page_type = struct
  type t = Data_page [@@deriving sexp_of, equal]

  let to_thrift Data_page = enum 0
end
