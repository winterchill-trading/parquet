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

(* Struct builder: keeps only the fields that are present. *)
let struct_ fields = Thrift_compact.Struct (List.filter_opt fields)
let req id v = Some (id, v)
let opt id f v = Option.map v ~f:(fun v -> id, f v)
let empty_struct = Thrift_compact.Struct []

module Time_unit = struct
  type t =
    | Millis
    | Micros
    | Nanos
  [@@deriving sexp_of, equal]

  (* union TimeUnit { 1: MILLIS; 2: MICROS; 3: NANOS } *)
  let to_thrift t =
    let id =
      match t with
      | Millis -> 1
      | Micros -> 2
      | Nanos -> 3
    in
    Thrift_compact.Struct [ id, empty_struct ]
  ;;
end

module Logical_type = struct
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

  (* union LogicalType { 1: STRING; 5: DECIMAL; 8: TIMESTAMP; ... } *)
  let to_thrift t =
    Thrift_compact.Struct
      [ (match t with
         | String -> 1, empty_struct
         | Decimal { scale; precision } ->
           5, Struct [ 1, Thrift_compact.i32 scale; 2, Thrift_compact.i32 precision ]
         | Timestamp { is_adjusted_to_utc; unit } ->
           8, Struct [ 1, Bool is_adjusted_to_utc; 2, Time_unit.to_thrift unit ])
      ]
  ;;
end

module Statistics = struct
  type t =
    { null_count : int option
    ; min_value : string option
    ; max_value : string option
    }
  [@@deriving sexp_of]

  let to_thrift { null_count; min_value; max_value } =
    struct_
      [ opt 3 Thrift_compact.i64 null_count
      ; opt 5 Thrift_compact.binary max_value
      ; opt 6 Thrift_compact.binary min_value
      ]
  ;;
end

module Schema_element = struct
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

  let to_thrift t =
    struct_
      [ opt 1 Physical_type.to_thrift t.type_
      ; opt 2 Thrift_compact.i32 t.type_length
      ; opt 3 Repetition.to_thrift t.repetition_type
      ; req 4 (Thrift_compact.binary t.name)
      ; opt 5 Thrift_compact.i32 t.num_children
      ; opt 6 Converted_type.to_thrift t.converted_type
      ; opt 7 Thrift_compact.i32 t.scale
      ; opt 8 Thrift_compact.i32 t.precision
      ; opt 10 Logical_type.to_thrift t.logical_type
      ]
  ;;
end

module Data_page_header = struct
  type t =
    { num_values : int
    ; encoding : Encoding.t
    ; definition_level_encoding : Encoding.t
    ; repetition_level_encoding : Encoding.t
    ; statistics : Statistics.t option
    }
  [@@deriving sexp_of]

  let to_thrift t =
    struct_
      [ req 1 (Thrift_compact.i32 t.num_values)
      ; req 2 (Encoding.to_thrift t.encoding)
      ; req 3 (Encoding.to_thrift t.definition_level_encoding)
      ; req 4 (Encoding.to_thrift t.repetition_level_encoding)
      ; opt 5 Statistics.to_thrift t.statistics
      ]
  ;;
end

module Page_header = struct
  type t =
    { type_ : Page_type.t
    ; uncompressed_page_size : int
    ; compressed_page_size : int
    ; data_page_header : Data_page_header.t option
    }
  [@@deriving sexp_of]

  let to_thrift t =
    struct_
      [ req 1 (Page_type.to_thrift t.type_)
      ; req 2 (Thrift_compact.i32 t.uncompressed_page_size)
      ; req 3 (Thrift_compact.i32 t.compressed_page_size)
      ; opt 5 Data_page_header.to_thrift t.data_page_header
      ]
  ;;
end

let list elem_type f xs = Thrift_compact.List (elem_type, List.map xs ~f)

module Column_meta_data = struct
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

  let to_thrift t =
    struct_
      [ req 1 (Physical_type.to_thrift t.type_)
      ; req 2 (list I32 Encoding.to_thrift t.encodings)
      ; req 3 (list Binary Thrift_compact.binary t.path_in_schema)
      ; req 4 (Compression_codec.to_thrift t.codec)
      ; req 5 (Thrift_compact.i64 t.num_values)
      ; req 6 (Thrift_compact.i64 t.total_uncompressed_size)
      ; req 7 (Thrift_compact.i64 t.total_compressed_size)
      ; req 9 (Thrift_compact.i64 t.data_page_offset)
      ; opt 12 Statistics.to_thrift t.statistics
      ]
  ;;
end

module Column_chunk = struct
  type t =
    { file_offset : int
    ; meta_data : Column_meta_data.t
    }
  [@@deriving sexp_of]

  let to_thrift t =
    struct_
      [ req 2 (Thrift_compact.i64 t.file_offset)
      ; req 3 (Column_meta_data.to_thrift t.meta_data)
      ]
  ;;
end

module Row_group = struct
  type t =
    { columns : Column_chunk.t list
    ; total_byte_size : int
    ; num_rows : int
    ; file_offset : int option
    ; total_compressed_size : int option
    ; ordinal : int option
    }
  [@@deriving sexp_of]

  let to_thrift t =
    struct_
      [ req 1 (list Struct Column_chunk.to_thrift t.columns)
      ; req 2 (Thrift_compact.i64 t.total_byte_size)
      ; req 3 (Thrift_compact.i64 t.num_rows)
      ; opt 5 Thrift_compact.i64 t.file_offset
      ; opt 6 Thrift_compact.i64 t.total_compressed_size
      ; opt 7 (fun n -> Thrift_compact.I16 n) t.ordinal
      ]
  ;;
end

module File_meta_data = struct
  type t =
    { version : int
    ; schema : Schema_element.t list
    ; num_rows : int
    ; row_groups : Row_group.t list
    ; created_by : string option
    ; type_defined_order : bool
    }
  [@@deriving sexp_of]

  (* union ColumnOrder { 1: TypeDefinedOrder TYPE_ORDER } *)
  let type_order = Thrift_compact.Struct [ 1, empty_struct ]

  let to_thrift t =
    let leaves =
      List.count t.schema ~f:(fun (e : Schema_element.t) -> Option.is_none e.num_children)
    in
    struct_
      [ req 1 (Thrift_compact.i32 t.version)
      ; req 2 (list Struct Schema_element.to_thrift t.schema)
      ; req 3 (Thrift_compact.i64 t.num_rows)
      ; req 4 (list Struct Row_group.to_thrift t.row_groups)
      ; opt 6 Thrift_compact.binary t.created_by
      ; Option.some_if
          t.type_defined_order
          (7, Thrift_compact.List (Struct, List.init leaves ~f:(fun _ -> type_order)))
      ]
  ;;
end
