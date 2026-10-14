open! Core

let enum n = Thrift_compact.i32 n

(* Reading helpers. Every [of_thrift] raises through [malformed] on input that does not
   match parquet.thrift. *)

let[@cold] malformed what (v : Thrift_compact.t) =
  raise_s [%message "Parquet.Metadata: malformed" (what : string) (v : Thrift_compact.t)]
;;

let fields what : Thrift_compact.t -> (int * Thrift_compact.t) list = function
  | Struct fields -> fields
  | v -> malformed what v
;;

let find fields id = List.Assoc.find fields id ~equal:Int.equal

let get_int what : Thrift_compact.t -> int = function
  | Byte n | I16 n -> n
  | I32 n -> Int32.to_int_exn n
  | I64 n ->
    (match Int64.to_int n with
     | Some n -> n
     | None -> malformed what (I64 n))
  | v -> malformed what v
;;

let get_string what : Thrift_compact.t -> string = function
  | Binary s -> s
  | v -> malformed what v
;;

let get_bool what : Thrift_compact.t -> bool = function
  | Bool b -> b
  | v -> malformed what v
;;

let get_list what f : Thrift_compact.t -> _ list = function
  | List (_, elems) -> List.map elems ~f
  | v -> malformed what v
;;

let required fields id what f =
  match find fields id with
  | Some v -> f v
  | None -> malformed (sprintf "missing field %d (%s)" id what) (Struct fields)
;;

let optional fields id f = Option.map (find fields id) ~f

(* An enum read from its i32 code with a total [of_int]. *)
let enum_of what of_int v =
  let n = get_int what v in
  match of_int n with
  | Some t -> t
  | None -> malformed (sprintf "unknown %s %d" what n) v
;;

module Physical_type = struct
  type t =
    | Boolean
    | Int32
    | Int64
    | Int96
    | Float
    | Double
    | Byte_array
    | Fixed_len_byte_array
  [@@deriving sexp_of, equal]

  let to_int = function
    | Boolean -> 0
    | Int32 -> 1
    | Int64 -> 2
    | Int96 -> 3
    | Float -> 4
    | Double -> 5
    | Byte_array -> 6
    | Fixed_len_byte_array -> 7
  ;;

  let all =
    [ Boolean; Int32; Int64; Int96; Float; Double; Byte_array; Fixed_len_byte_array ]
  ;;

  let of_int n = List.find all ~f:(fun t -> to_int t = n)
  let to_thrift t = enum (to_int t)
  let of_thrift v = enum_of "physical type" of_int v
end

module Converted_type = struct
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
  [@@deriving sexp_of, equal, enumerate]

  (* The constructors are in thrift order, 0 to 21. *)
  let to_int t = fst (List.findi_exn all ~f:(fun _ x -> equal x t))
  let of_int n = List.nth all n
  let to_thrift t = enum (to_int t)
  let of_thrift v = enum_of "converted type" of_int v
end

module Repetition = struct
  type t =
    | Required
    | Optional
    | Repeated
  [@@deriving sexp_of, equal]

  let to_int = function
    | Required -> 0
    | Optional -> 1
    | Repeated -> 2
  ;;

  let of_int = function
    | 0 -> Some Required
    | 1 -> Some Optional
    | 2 -> Some Repeated
    | _ -> None
  ;;

  let to_thrift t = enum (to_int t)
  let of_thrift v = enum_of "repetition" of_int v
end

module Encoding = struct
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

  let to_int = function
    | Plain -> 0
    | Plain_dictionary -> 2
    | Rle -> 3
    | Bit_packed -> 4
    | Delta_binary_packed -> 5
    | Delta_length_byte_array -> 6
    | Delta_byte_array -> 7
    | Rle_dictionary -> 8
    | Byte_stream_split -> 9
  ;;

  let all =
    [ Plain
    ; Plain_dictionary
    ; Rle
    ; Bit_packed
    ; Delta_binary_packed
    ; Delta_length_byte_array
    ; Delta_byte_array
    ; Rle_dictionary
    ; Byte_stream_split
    ]
  ;;

  let of_int n = List.find all ~f:(fun t -> to_int t = n)
  let to_thrift t = enum (to_int t)
  let of_thrift v = enum_of "encoding" of_int v
end

module Compression_codec = struct
  type t =
    | Uncompressed
    | Snappy
    | Gzip
    | Lzo
    | Brotli
    | Lz4
    | Zstd
    | Lz4_raw
  [@@deriving sexp_of, equal, enumerate]

  (* The constructors are in thrift order, 0 to 7. *)
  let to_int t = fst (List.findi_exn all ~f:(fun _ x -> equal x t))
  let of_int n = List.nth all n
  let to_thrift t = enum (to_int t)
  let of_thrift v = enum_of "compression codec" of_int v
end

module Page_type = struct
  type t =
    | Data_page
    | Index_page
    | Dictionary_page
    | Data_page_v2
  [@@deriving sexp_of, equal, enumerate]

  (* The constructors are in thrift order, 0 to 3. *)
  let to_int t = fst (List.findi_exn all ~f:(fun _ x -> equal x t))
  let of_int n = List.nth all n
  let to_thrift t = enum (to_int t)
  let of_thrift v = enum_of "page type" of_int v
end

(* Struct builder: keeps only the fields that are present. *)
let struct_ fields = Thrift_compact.Struct (List.filter_opt fields)
let req id v = Some (id, v)
let opt id f v = Option.map v ~f:(fun v -> id, f v)
let empty_struct = Thrift_compact.Struct []

(* The single field of a Thrift union. *)
let union_field what v =
  match fields what v with
  | [ field ] -> field
  | _ -> malformed what v
;;

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

  let of_thrift v =
    match union_field "time unit" v with
    | 1, _ -> Millis
    | 2, _ -> Micros
    | 3, _ -> Nanos
    | _ -> malformed "time unit" v
  ;;
end

module Logical_type = struct
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

  (* union LogicalType
     [{ 1: STRING; 2: MAP; 3: LIST; 4: ENUM; 5: DECIMAL; 6: DATE; 7: TIME; 8: TIMESTAMP; 10: INTEGER; 11: UNKNOWN; 12: JSON; 13: BSON; 14: UUID; 15: FLOAT16; ... }] *)
  let to_thrift t =
    let adjusted is_adjusted_to_utc unit =
      Thrift_compact.Struct [ 1, Bool is_adjusted_to_utc; 2, Time_unit.to_thrift unit ]
    in
    Thrift_compact.Struct
      [ (match t with
         | String -> 1, empty_struct
         | Map -> 2, empty_struct
         | List -> 3, empty_struct
         | Enum -> 4, empty_struct
         | Decimal { scale; precision } ->
           5, Struct [ 1, Thrift_compact.i32 scale; 2, Thrift_compact.i32 precision ]
         | Date -> 6, empty_struct
         | Time { is_adjusted_to_utc; unit } -> 7, adjusted is_adjusted_to_utc unit
         | Timestamp { is_adjusted_to_utc; unit } -> 8, adjusted is_adjusted_to_utc unit
         | Integer { bit_width; is_signed } ->
           10, Struct [ 1, Byte bit_width; 2, Bool is_signed ]
         | Unknown -> 11, empty_struct
         | Json -> 12, empty_struct
         | Bson -> 13, empty_struct
         | Uuid -> 14, empty_struct
         | Float16 -> 15, empty_struct
         | Other id -> id, empty_struct)
      ]
  ;;

  let of_thrift v =
    let id, body = union_field "logical type" v in
    let body () = fields "logical type" body in
    let adjusted () =
      let f = body () in
      ( required f 1 "isAdjustedToUTC" (get_bool "isAdjustedToUTC")
      , required f 2 "unit" Time_unit.of_thrift )
    in
    match id with
    | 1 -> String
    | 2 -> Map
    | 3 -> List
    | 4 -> Enum
    | 5 ->
      let f = body () in
      Decimal
        { scale = required f 1 "scale" (get_int "scale")
        ; precision = required f 2 "precision" (get_int "precision")
        }
    | 6 -> Date
    | 7 ->
      let is_adjusted_to_utc, unit = adjusted () in
      Time { is_adjusted_to_utc; unit }
    | 8 ->
      let is_adjusted_to_utc, unit = adjusted () in
      Timestamp { is_adjusted_to_utc; unit }
    | 10 ->
      let f = body () in
      Integer
        { bit_width = required f 1 "bitWidth" (get_int "bitWidth")
        ; is_signed = required f 2 "isSigned" (get_bool "isSigned")
        }
    | 11 -> Unknown
    | 12 -> Json
    | 13 -> Bson
    | 14 -> Uuid
    | 15 -> Float16
    | id -> Other id
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

  let of_thrift v =
    let f = fields "statistics" v in
    { null_count = optional f 3 (get_int "null_count")
    ; max_value = optional f 5 (get_string "max_value")
    ; min_value = optional f 6 (get_string "min_value")
    }
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

  let of_thrift v =
    let f = fields "schema element" v in
    { type_ = optional f 1 Physical_type.of_thrift
    ; type_length = optional f 2 (get_int "type_length")
    ; repetition_type = optional f 3 Repetition.of_thrift
    ; name = required f 4 "name" (get_string "name")
    ; num_children = optional f 5 (get_int "num_children")
    ; converted_type = optional f 6 Converted_type.of_thrift
    ; scale = optional f 7 (get_int "scale")
    ; precision = optional f 8 (get_int "precision")
    ; logical_type = optional f 10 Logical_type.of_thrift
    }
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

  let of_thrift v =
    let f = fields "data page header" v in
    { num_values = required f 1 "num_values" (get_int "num_values")
    ; encoding = required f 2 "encoding" Encoding.of_thrift
    ; definition_level_encoding =
        required f 3 "definition_level_encoding" Encoding.of_thrift
    ; repetition_level_encoding =
        required f 4 "repetition_level_encoding" Encoding.of_thrift
    ; statistics = optional f 5 Statistics.of_thrift
    }
  ;;
end

module Dictionary_page_header = struct
  type t =
    { num_values : int
    ; encoding : Encoding.t
    ; is_sorted : bool option
    }
  [@@deriving sexp_of]

  let to_thrift t =
    struct_
      [ req 1 (Thrift_compact.i32 t.num_values)
      ; req 2 (Encoding.to_thrift t.encoding)
      ; opt 3 (fun b -> Thrift_compact.Bool b) t.is_sorted
      ]
  ;;

  let of_thrift v =
    let f = fields "dictionary page header" v in
    { num_values = required f 1 "num_values" (get_int "num_values")
    ; encoding = required f 2 "encoding" Encoding.of_thrift
    ; is_sorted = optional f 3 (get_bool "is_sorted")
    }
  ;;
end

module Page_header = struct
  type t =
    { type_ : Page_type.t
    ; uncompressed_page_size : int
    ; compressed_page_size : int
    ; data_page_header : Data_page_header.t option
    ; dictionary_page_header : Dictionary_page_header.t option
    }
  [@@deriving sexp_of]

  let to_thrift t =
    struct_
      [ req 1 (Page_type.to_thrift t.type_)
      ; req 2 (Thrift_compact.i32 t.uncompressed_page_size)
      ; req 3 (Thrift_compact.i32 t.compressed_page_size)
      ; opt 5 Data_page_header.to_thrift t.data_page_header
      ; opt 7 Dictionary_page_header.to_thrift t.dictionary_page_header
      ]
  ;;

  let of_thrift v =
    let f = fields "page header" v in
    { type_ = required f 1 "type" Page_type.of_thrift
    ; uncompressed_page_size =
        required f 2 "uncompressed_page_size" (get_int "uncompressed_page_size")
    ; compressed_page_size =
        required f 3 "compressed_page_size" (get_int "compressed_page_size")
    ; data_page_header = optional f 5 Data_page_header.of_thrift
    ; dictionary_page_header = optional f 7 Dictionary_page_header.of_thrift
    }
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
    ; dictionary_page_offset : int option
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
      ; opt 11 Thrift_compact.i64 t.dictionary_page_offset
      ; opt 12 Statistics.to_thrift t.statistics
      ]
  ;;

  let of_thrift v =
    let f = fields "column metadata" v in
    let int id what = required f id what (get_int what) in
    { type_ = required f 1 "type" Physical_type.of_thrift
    ; encodings = required f 2 "encodings" (get_list "encodings" Encoding.of_thrift)
    ; path_in_schema =
        required f 3 "path_in_schema" (get_list "path_in_schema" (get_string "path"))
    ; codec = required f 4 "codec" Compression_codec.of_thrift
    ; num_values = int 5 "num_values"
    ; total_uncompressed_size = int 6 "total_uncompressed_size"
    ; total_compressed_size = int 7 "total_compressed_size"
    ; data_page_offset = int 9 "data_page_offset"
    ; dictionary_page_offset = optional f 11 (get_int "dictionary_page_offset")
    ; statistics = optional f 12 Statistics.of_thrift
    }
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

  let of_thrift v =
    let f = fields "column chunk" v in
    (match find f 1 with
     | Some path -> malformed "column chunk in another file" path
     | None -> ());
    { file_offset = required f 2 "file_offset" (get_int "file_offset")
    ; meta_data = required f 3 "meta_data" Column_meta_data.of_thrift
    }
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

  let of_thrift v =
    let f = fields "row group" v in
    { columns = required f 1 "columns" (get_list "columns" Column_chunk.of_thrift)
    ; total_byte_size = required f 2 "total_byte_size" (get_int "total_byte_size")
    ; num_rows = required f 3 "num_rows" (get_int "num_rows")
    ; file_offset = optional f 5 (get_int "file_offset")
    ; total_compressed_size = optional f 6 (get_int "total_compressed_size")
    ; ordinal = optional f 7 (get_int "ordinal")
    }
  ;;
end

module Key_value = struct
  type t =
    { key : string
    ; value : string option
    }
  [@@deriving sexp_of, equal]

  let to_thrift t =
    struct_ [ req 1 (Thrift_compact.binary t.key); opt 2 Thrift_compact.binary t.value ]
  ;;

  let of_thrift v =
    let f = fields "key value" v in
    { key = required f 1 "key" (get_string "key")
    ; value = optional f 2 (get_string "value")
    }
  ;;
end

module File_meta_data = struct
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
      ; Option.some_if
          (not (List.is_empty t.key_value_metadata))
          (5, list Struct Key_value.to_thrift t.key_value_metadata)
      ; opt 6 Thrift_compact.binary t.created_by
      ; Option.some_if
          t.type_defined_order
          (7, Thrift_compact.List (Struct, List.init leaves ~f:(fun _ -> type_order)))
      ]
  ;;

  let of_thrift v =
    let f = fields "file metadata" v in
    { version = required f 1 "version" (get_int "version")
    ; schema = required f 2 "schema" (get_list "schema" Schema_element.of_thrift)
    ; num_rows = required f 3 "num_rows" (get_int "num_rows")
    ; row_groups = required f 4 "row_groups" (get_list "row_groups" Row_group.of_thrift)
    ; key_value_metadata =
        Option.value
          (optional f 5 (get_list "key_value_metadata" Key_value.of_thrift))
          ~default:[]
    ; created_by = optional f 6 (get_string "created_by")
    ; type_defined_order =
        (match find f 7 with
         | None -> false
         | Some orders ->
           get_list "column_orders" (fun o -> union_field "column order" o) orders
           |> List.for_all ~f:(fun (id, _) -> id = 1))
    }
  ;;
end
