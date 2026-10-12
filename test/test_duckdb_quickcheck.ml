open! Core
open Parquet

type row =
  { a : int
  ; b : int
  ; s : string
  ; flag : bool
  }
[@@deriving sexp_of]

let quickcheck_generator_row =
  let open Quickcheck.Generator.Let_syntax in
  let%map a = Int.gen_incl (-0x8000_0000) 0x7FFF_FFFF
  and b = Int.quickcheck_generator
  and s = String.gen_with_length 5 Char.gen_alphanum
  and flag = Bool.quickcheck_generator in
  { a; b; s; flag }
;;

let schema =
  Schema.create
    Schema.Column.
      [ int32 "a" (fun r -> r.a)
      ; int64 "b" (fun r -> r.b)
      ; utf8 "s" (fun r -> r.s)
      ; bool "flag" (fun r -> r.flag)
      ]
;;

let duckdb_csv file =
  Duckdb.with_output (fun () ->
    Duckdb.query
      ~file
      "select a, b, s, flag from read_parquet($file, file_row_number = true) order by \
       file_row_number")
;;

let expected_csv rows =
  "a,b,s,flag\n"
  ^ String.concat (List.map rows ~f:(fun r -> sprintf "%d,%d,%s,%b\n" r.a r.b r.s r.flag))
;;

let%expect_test "random files read back identically" =
  let gen =
    Quickcheck.Generator.tuple3
      (List.gen_with_length 50 quickcheck_generator_row
       |> Quickcheck.Generator.bind ~f:(fun rows ->
         Quickcheck.Generator.map (Int.gen_incl 0 50) ~f:(fun n -> List.take rows n)))
      (Int.gen_incl 1 20)
      Bool.quickcheck_generator
  in
  Quickcheck.test
    gen
    ~trials:25
    ~sexp_of:[%sexp_of: row list * int * bool]
    ~f:(fun (rows, row_group_rows, zstd) ->
      Duckdb.with_parquet_file (fun file ->
        Writer.with_file
          ~schema
          ~row_group_rows
          ~compression:(if zstd then `Zstd else `None)
          file
          ~f:(fun w -> Writer.add_all w rows);
        [%test_result: string] (duckdb_csv file) ~expect:(expected_csv rows)));
  [%expect {| |}]
;;
