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

module Time_unit : sig
  type t =
    | Millis
    | Micros
    | Nanos
  [@@deriving sexp_of, equal]
end

module Logical_type : sig
  type t =
    | String
    | Decimal of
        { scale : int
        ; precision : int
        }
    | Timestamp of
        { is_adjusted_to_utc : bool
        ; unit : Time_unit.t
        }
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
end

module Statistics : sig
  (** Min and max are the PLAIN encodings of the values, without the length prefix for
      byte arrays. *)
  type t =
    { null_count : int option
    ; min_value : string option
    ; max_value : string option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module Schema_element : sig
  type t =
    { type_ : Physical_type.t option
    ; type_length : int option
    ; repetition_type : Repetition.t option
    ; name : string
    ; num_children : int option
    ; converted_type : Converted_type.t option
    ; scale : int option
    ; precision : int option
    ; logical_type : Logical_type.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module Data_page_header : sig
  type t =
    { num_values : int
    ; encoding : Encoding.t
    ; definition_level_encoding : Encoding.t
    ; repetition_level_encoding : Encoding.t
    ; statistics : Statistics.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module Page_header : sig
  type t =
    { type_ : Page_type.t
    ; uncompressed_page_size : int
    ; compressed_page_size : int
    ; data_page_header : Data_page_header.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module Column_meta_data : sig
  type t =
    { type_ : Physical_type.t
    ; encodings : Encoding.t list
    ; path_in_schema : string list
    ; codec : Compression_codec.t
    ; num_values : int
    ; total_uncompressed_size : int
    ; total_compressed_size : int
    ; data_page_offset : int
    ; statistics : Statistics.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module Column_chunk : sig
  type t =
    { file_offset : int
    ; meta_data : Column_meta_data.t
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module Row_group : sig
  type t =
    { columns : Column_chunk.t list
    ; total_byte_size : int
    ; num_rows : int
    ; file_offset : int option
    ; total_compressed_size : int option
    ; ordinal : int option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end

module File_meta_data : sig
  (** [column_orders] is written as one [TYPE_ORDER] per leaf column when
      [type_defined_order] is set; readers need it to trust [min_value] and [max_value]. *)
  type t =
    { version : int
    ; schema : Schema_element.t list
    ; num_rows : int
    ; row_groups : Row_group.t list
    ; created_by : string option
    ; type_defined_order : bool
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
end
