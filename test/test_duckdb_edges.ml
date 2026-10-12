open! Core
open Parquet

let%expect_test "integer extremes" =
  let schema =
    Schema.create
      Schema.Column.
        [ int32 "i32" fst; int64 "i64" snd; decimal ~precision:18 ~scale:2 "dec" snd ]
  in
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      Writer.add_all
        w
        [ Int32.(to_int_exn min_value), Int.min_value
        ; Int32.(to_int_exn max_value), Int.max_value
        ; -1, -1
        ; 0, 0
        ]);
    Duckdb.query ~file "select * from $file");
  [%expect
    {|
    i32,i64,dec
    -2147483648,-4611686018427387904,-46116860184273879.04
    2147483647,4611686018427387903,46116860184273879.03
    -1,-1,-0.01
    0,0,0.00
    |}]
;;

let%expect_test "special doubles" =
  let schema = Schema.create Schema.Column.[ float "x" Fn.id ] in
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      Writer.add_all w [ Float.nan; Float.infinity; Float.neg_infinity; -0.; 1e-300 ]);
    Duckdb.query ~file "select x from $file";
    (* NaN is left out of the statistics; DuckDB shows infinite bounds as NULL but must
       not prune with them. *)
    Duckdb.query
      ~file
      "select stats_min_value, stats_max_value from parquet_metadata($file)";
    Duckdb.query ~file "select count(*) from $file where x > 1e300");
  [%expect
    {|
    x
    nan
    inf
    -inf
    -0.0
    1e-300
    stats_min_value,stats_max_value
    NULL,NULL
    count_star()
    2
    |}]
;;

let%expect_test "strings: empty, multi-byte, long" =
  let schema = Schema.create Schema.Column.[ utf8 "s" fst; bytes "b" snd ] in
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      Writer.add_all
        w
        [ "", "\xff\x00"
        ; "日本語", ""
        ; "a,b", "a,b"
        ; String.make 10_000 'z', String.make 70_000 '\000'
        ]);
    Duckdb.query ~file "select length(s), octet_length(b), left(s, 5) from $file");
  [%expect
    {|
    length(s),octet_length(b),"""left""(s, 5)"
    0,2,
    3,0,"日本語"
    3,3,"a,b"
    10000,70000,zzzzz
    |}]
;;

let%expect_test "booleans that do not fill the last byte" =
  let schema = Schema.create Schema.Column.[ bool "b" Fn.id ] in
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema ~row_group_rows:5 file ~f:(fun w ->
      Writer.add_all w (List.init 13 ~f:(fun i -> i % 2 = 0 || i = 7)));
    Duckdb.query
      ~file
      "select string_agg(b::int::varchar, '' order by file_row_number) from \
       read_parquet($file, file_row_number = true)");
  [%expect
    {|
    "string_agg(CAST(CAST(b AS INTEGER) AS VARCHAR), '' ORDER BY file_row_number)"
    1010101110101
    |}]
;;
