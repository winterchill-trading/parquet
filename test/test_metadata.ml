open! Core
open Parquet.Metadata

let print t = Hex.print (Parquet.Thrift_compact.to_string t)

let element ?type_ ?repetition_type ?converted_type ?scale ?precision ?logical_type name
  : Schema_element.t
  =
  { type_
  ; type_length = None
  ; repetition_type
  ; name
  ; num_children = None
  ; converted_type
  ; scale
  ; precision
  ; logical_type
  }
;;

let%expect_test "timestamp nanos schema element" =
  print
    (Schema_element.to_thrift
       (element
          "ts"
          ~type_:Int64
          ~repetition_type:Required
          ~logical_type:(Timestamp { is_adjusted_to_utc = true; unit = Nanos })));
  (* type=INT64 (1), repetition=REQUIRED (3), name (4), logicalType (10) -> TIMESTAMP (8)
     -> [{ isAdjustedToUTC=true, unit -> NANOS (3) }] *)
  [%expect {| 15 04 25 00 18 02 74 73 6c 8c 11 1c 3c 00 00 00 00 00 |}]
;;

let%expect_test "decimal schema element sets the converted type too" =
  print
    (Schema_element.to_thrift
       (element
          "px"
          ~type_:Int64
          ~repetition_type:Required
          ~converted_type:Decimal
          ~scale:4
          ~precision:18
          ~logical_type:(Decimal { scale = 4; precision = 18 })));
  [%expect {| 15 04 25 00 18 02 70 78 25 0a 15 08 15 24 2c 5c 15 08 15 24 00 00 00 |}]
;;

let%expect_test "root schema element" =
  print (Schema_element.to_thrift { (element "schema") with num_children = Some 2 });
  [%expect {| 48 06 73 63 68 65 6d 61 15 04 00 |}]
;;

let%expect_test "data page header with statistics" =
  print
    (Page_header.to_thrift
       { type_ = Data_page
       ; uncompressed_page_size = 16
       ; compressed_page_size = 16
       ; data_page_header =
           Some
             { num_values = 2
             ; encoding = Plain
             ; definition_level_encoding = Rle
             ; repetition_level_encoding = Rle
             ; statistics =
                 Some { null_count = Some 0; min_value = Some "a"; max_value = Some "z" }
             }
       ; dictionary_page_header = None
       ; data_page_header_v2 = None
       });
  [%expect
    {| 15 00 15 20 15 20 2c 15 04 15 00 15 06 15 06 1c 36 00 28 01 7a 18 01 61 00 00 00 |}]
;;

(* [of_thrift] must read back what [to_thrift] writes. *)
let round_trip to_thrift of_thrift sexp_of t =
  let back =
    of_thrift
      (Parquet.Thrift_compact.of_string (Parquet.Thrift_compact.to_string (to_thrift t)))
  in
  print_s [%sexp (Sexp.equal (sexp_of back) (sexp_of t) : bool)]
;;

let%expect_test "of_thrift inverts to_thrift" =
  List.iter
    [ element
        "ts"
        ~type_:Int64
        ~repetition_type:Optional
        ~logical_type:(Timestamp { is_adjusted_to_utc = false; unit = Nanos })
    ; element
        "px"
        ~type_:Int64
        ~converted_type:Decimal
        ~scale:2
        ~precision:9
        ~logical_type:(Decimal { scale = 2; precision = 9 })
    ; element
        "i8"
        ~type_:Int32
        ~converted_type:Int_8
        ~logical_type:(Integer { bit_width = 8; is_signed = true })
    ; element
        "t"
        ~type_:Int64
        ~logical_type:(Time { is_adjusted_to_utc = true; unit = Micros })
    ; element "u" ~type_:Fixed_len_byte_array ~logical_type:Uuid
    ; element "o" ~type_:Byte_array ~logical_type:(Other 16)
    ]
    ~f:
      (round_trip
         Schema_element.to_thrift
         Schema_element.of_thrift
         Schema_element.sexp_of_t);
  round_trip
    Page_header.to_thrift
    Page_header.of_thrift
    Page_header.sexp_of_t
    { type_ = Dictionary_page
    ; uncompressed_page_size = 10
    ; compressed_page_size = 7
    ; data_page_header = None
    ; dictionary_page_header =
        Some { num_values = 3; encoding = Plain_dictionary; is_sorted = Some false }
    ; data_page_header_v2 = None
    };
  round_trip
    File_meta_data.to_thrift
    File_meta_data.of_thrift
    File_meta_data.sexp_of_t
    { version = 1
    ; schema =
        [ { (element "schema") with num_children = Some 1 }; element "a" ~type_:Boolean ]
    ; num_rows = 5
    ; row_groups =
        [ { columns =
              [ { file_offset = 4
                ; meta_data =
                    { type_ = Boolean
                    ; encodings = [ Plain; Rle; Rle_dictionary ]
                    ; path_in_schema = [ "a" ]
                    ; codec = Lz4_raw
                    ; num_values = 5
                    ; total_uncompressed_size = 20
                    ; total_compressed_size = 20
                    ; data_page_offset = 4
                    ; dictionary_page_offset = Some 4
                    ; statistics =
                        Some { null_count = Some 1; min_value = None; max_value = None }
                    }
                }
              ]
          ; total_byte_size = 20
          ; num_rows = 5
          ; file_offset = Some 4
          ; total_compressed_size = Some 20
          ; ordinal = Some 0
          }
        ]
    ; key_value_metadata =
        [ { key = "k"; value = Some "v" }; { key = "flag"; value = None } ]
    ; created_by = Some "test"
    ; type_defined_order = true
    };
  [%expect
    {|
    true
    true
    true
    true
    true
    true
    true
    true
    |}]
;;

let%expect_test "malformed metadata is reported" =
  let show v =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> Schema_element.of_thrift v)
         : Schema_element.t Or_error.t)]
  in
  show (Struct [ 1, Parquet.Thrift_compact.i32 1 ]);
  show (Struct [ 1, Parquet.Thrift_compact.i32 42; 4, Binary "x" ]);
  show (Binary "x");
  [%expect
    {|
    (Error
     ("Parquet.Metadata: malformed" (what "missing field 4 (name)")
      (v (Struct ((1 (I32 1)))))))
    (Error
     ("Parquet.Metadata: malformed" (what "unknown physical type 42")
      (v (I32 42))))
    (Error
     ("Parquet.Metadata: malformed" (what "schema element") (v (Binary x))))
    |}]
;;
