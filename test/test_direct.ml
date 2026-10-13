open! Core
open Parquet

type row =
  { n : int
  ; x : float
  }

let schema =
  Schema.create Schema.Column.[ int32 "n" (fun r -> r.n); float "x" (fun r -> r.x) ]
;;

let show f = print_s [%sexp (Result.try_with f : (unit, exn) Result.t)]

let%expect_test "column lookups check the name and the type" =
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      show (fun () -> ignore (Writer.Direct.int w "missing" : Writer.Direct.int_column));
      show (fun () -> ignore (Writer.Direct.float w "n" : Writer.Direct.float_column));
      show (fun () -> ignore (Writer.Direct.int w "n" : Writer.Direct.int_column))));
  [%expect {|
    (Error ("Parquet.Writer.Direct: no such column" (name missing)))
    (Error
     ("Parquet.Writer.Direct: column has another type" (name n) (kind float)))
    (Ok ())
    |}]
;;

let%expect_test "end_row needs one value per column" =
  Duckdb.with_parquet_file (fun file ->
    let w = Writer.create ~schema file in
    let n = Writer.Direct.int w "n"
    and x = Writer.Direct.float w "x" in
    Writer.Direct.add_int n 1;
    show (fun () -> Writer.Direct.end_row w);
    Writer.Direct.add_float x 1.;
    Writer.Direct.add_float x 2.;
    show (fun () -> Writer.Direct.end_row w));
  [%expect {|
    (Error
     ("Parquet.Writer.Direct.end_row: a column needs exactly one value per row"
      (name x) (values 0)))
    (Error
     ("Parquet.Writer.Direct.end_row: a column needs exactly one value per row"
      (name x) (values 2)))
    |}]
;;

let%expect_test "int32 range is checked" =
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      show (fun () -> Writer.Direct.add_int (Writer.Direct.int w "n") (1 lsl 40))));
  [%expect {| (Error ("Parquet.Encoding.int32: out of range" (n 1099511627776))) |}]
;;

let%expect_test "rows read back with DuckDB" =
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      let n = Writer.Direct.int w "n"
      and x = Writer.Direct.float w "x" in
      for i = 1 to 3 do
        Writer.Direct.add_int n i;
        Writer.Direct.add_float x (Float.of_int i /. 4.);
        Writer.Direct.end_row w
      done);
    Duckdb.query ~file "select n, x from $file");
  [%expect {|
    n,x
    1,0.25
    2,0.5
    3,0.75
    |}]
;;
