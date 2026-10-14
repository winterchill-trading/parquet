(** The Parquet file metadata ([parquet.thrift], vendored under [docs/]): the structs the
    writer produces and the reader consumes, with their Thrift compact serialization.

    Field ids follow [parquet.thrift]. Each [to_thrift] lists only the fields that are
    set. Each [of_thrift] reads the fields it models and ignores the others (crc, page
    indexes, bloom filters, encryption, sorting columns...).

    Every [of_thrift] raises if a required field is missing or a value has the wrong
    Thrift type or an unknown enum code. *)

open! Core

module Physical_type : sig
  type t =
    | Boolean
    | Int32
    | Int64
    | Int96 (** Deprecated; only read. *)
    | Float
    | Double
    | Byte_array
    | Fixed_len_byte_array
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Converted_type : sig
  (** The legacy annotation, written next to the logical type for older readers. *)
  type t =
    | Utf8
    | Map
    | Map_key_value
    | List
    | Enum
    | Decimal
    | Date
    | Time_millis
    | Time_micros
    | Timestamp_millis
    | Timestamp_micros
    | Uint_8
    | Uint_16
    | Uint_32
    | Uint_64
    | Int_8
    | Int_16
    | Int_32
    | Int_64
    | Json
    | Bson
    | Interval
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Repetition : sig
  type t =
    | Required
    | Optional
    | Repeated
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Encoding : sig
  type t =
    | Plain
    | Plain_dictionary
    | Rle
    | Bit_packed
    | Delta_binary_packed
    | Delta_length_byte_array
    | Delta_byte_array
    | Rle_dictionary
    | Byte_stream_split
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Compression_codec : sig
  type t =
    | Uncompressed
    | Snappy
    | Gzip
    | Lzo
    | Brotli
    | Lz4
    | Zstd
    | Lz4_raw
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Page_type : sig
  type t =
    | Data_page
    | Index_page
    | Dictionary_page
    | Data_page_v2
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Time_unit : sig
  type t =
    | Millis
    | Micros
    | Nanos
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Logical_type : sig
  (** [Other id] is a union member this module does not model (variant, geometry...);
      writing it produces an empty struct under [id]. *)
  type t =
    | String
    | Map
    | List
    | Enum
    | Decimal of
        { scale : int
        ; precision : int
        }
    | Date
    | Time of
        { is_adjusted_to_utc : bool
        ; unit : Time_unit.t
        }
    | Timestamp of
        { is_adjusted_to_utc : bool
        ; unit : Time_unit.t
        }
    | Integer of
        { bit_width : int
        ; is_signed : bool
        }
    | Unknown
    | Json
    | Bson
    | Uuid
    | Float16
    | Other of int
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Statistics : sig
  (** Min and max are the PLAIN encodings of the values, without the length prefix for
      byte arrays. The deprecated [min]/[max] fields (1 and 2) are not read. *)
  type t =
    { null_count : int option
    ; min_value : string option
    ; max_value : string option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
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
  val of_thrift : Thrift_compact.t -> t
end

module Data_page_header : sig
  type t =
    { num_values : int (** Including nulls. *)
    ; encoding : Encoding.t
    ; definition_level_encoding : Encoding.t
    ; repetition_level_encoding : Encoding.t
    ; statistics : Statistics.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Dictionary_page_header : sig
  type t =
    { num_values : int
    ; encoding : Encoding.t
    ; is_sorted : bool option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Data_page_header_v2 : sig
  (** Levels are stored uncompressed before the values, with their byte lengths here;
      [is_compressed] (default [true]) says whether the values section is. *)
  type t =
    { num_values : int
    ; num_nulls : int
    ; num_rows : int
    ; encoding : Encoding.t
    ; definition_levels_byte_length : int
    ; repetition_levels_byte_length : int
    ; is_compressed : bool
    ; statistics : Statistics.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Page_header : sig
  type t =
    { type_ : Page_type.t
    ; uncompressed_page_size : int
    ; compressed_page_size : int
    ; data_page_header : Data_page_header.t option
    ; dictionary_page_header : Dictionary_page_header.t option
    ; data_page_header_v2 : Data_page_header_v2.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
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
    ; dictionary_page_offset : int option
    ; statistics : Statistics.t option
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module Column_chunk : sig
  type t =
    { file_offset : int
    ; meta_data : Column_meta_data.t
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t

  (** Also raises on a chunk stored in another file ([file_path] set). *)
  val of_thrift : Thrift_compact.t -> t
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
  val of_thrift : Thrift_compact.t -> t
end

module Key_value : sig
  (** An application-defined entry of the file's key-value metadata. *)
  type t =
    { key : string
    ; value : string option
    }
  [@@deriving sexp_of, equal]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end

module File_meta_data : sig
  (** [column_orders] is written as one [TYPE_ORDER] per leaf column when
      [type_defined_order] is set; readers need it to trust [min_value] and [max_value].
      Reading sets [type_defined_order] when every column order is [TYPE_ORDER]. An empty
      [key_value_metadata] is not written. *)
  type t =
    { version : int
    ; schema : Schema_element.t list
    ; num_rows : int
    ; row_groups : Row_group.t list
    ; key_value_metadata : Key_value.t list
    ; created_by : string option
    ; type_defined_order : bool
    }
  [@@deriving sexp_of]

  val to_thrift : t -> Thrift_compact.t
  val of_thrift : Thrift_compact.t -> t
end
