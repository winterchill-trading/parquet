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
       });
  [%expect
    {| 15 00 15 20 15 20 2c 15 04 15 00 15 06 15 06 1c 36 00 28 01 7a 18 01 61 00 00 00 |}]
;;
