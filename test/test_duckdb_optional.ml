open! Core
open Parquet

(* Optional and dictionary-encoded columns, read back by DuckDB. *)

type row =
  { a : int option
  ; b : int option
  ; d : float option (* quarters, so DuckDB's text form is exact after [* 4] *)
  ; s : string option
  ; s2 : string
  ; flag : bool option
  ; ts : Time_ns.t option
  ; px : int option
  ; n : int
  }
[@@deriving sexp_of]

let schema =
  Schema.create
    Schema.Column.
      [ optional int32 "a" (fun r -> r.a)
      ; dictionary (optional int64 "b" (fun r -> r.b))
      ; optional float "d" (fun r -> r.d)
      ; dictionary (optional utf8 "s" (fun r -> r.s))
      ; dictionary (utf8 "s2" (fun r -> r.s2))
      ; optional bool "flag" (fun r -> r.flag)
      ; optional timestamp_ns "ts" (fun r -> r.ts)
      ; optional (decimal ~precision:9 ~scale:2) "px" (fun r -> r.px)
      ; dictionary (int64 "n" (fun r -> r.n))
      ]
;;

let gen_row =
  let open Quickcheck.Generator.Let_syntax in
  let opt g =
    Quickcheck.Generator.weighted_union [ 1., return None; 3., g >>| Option.some ]
  in
  let%map a = opt (Int.gen_incl (-0x8000_0000) 0x7FFF_FFFF)
  and b = opt (Quickcheck.Generator.of_list [ -5; 0; 7; Int.max_value; Int.min_value ])
  and d = opt (Int.gen_incl (-1000) 1000 >>| fun q -> Float.of_int q /. 4.)
  and s = opt (Quickcheck.Generator.of_list [ "BTC"; "ETH"; ""; "x y" ])
  and s2 = String.gen_with_length 2 (Quickcheck.Generator.of_list [ 'p'; 'q' ])
  and flag = opt Bool.quickcheck_generator
  and ts = opt (Int.gen_incl 0 Int.max_value >>| Time_ns.of_int_ns_since_epoch)
  and px = opt (Int.gen_incl (-999_999_999) 999_999_999)
  and n = Int.gen_incl 0 3 in
  { a; b; d; s; s2; flag; ts; px; n }
;;

let duckdb_csv file =
  Duckdb.with_output (fun () ->
    Duckdb.query
      ~file
      "select a, b, (d * 4)::bigint as d4, s, s2, flag, epoch_ns(ts) as ts, (px * \
       100)::bigint as px, n from read_parquet($file, file_row_number = true) order by \
       file_row_number")
;;

(* The DuckDB CLI's CSV mode prints a null as NULL and an empty string as nothing. *)
let field to_string = function
  | None -> "NULL"
  | Some v -> to_string v
;;

let expected_csv rows =
  "a,b,d4,s,s2,flag,ts,px,n\n"
  ^ String.concat
      (List.map rows ~f:(fun r ->
         let s = field Fn.id r.s in
         sprintf
           "%s,%s,%s,%s,%s,%s,%s,%s,%d\n"
           (field Int.to_string r.a)
           (field Int.to_string r.b)
           (field (fun d -> Int.to_string (Float.to_int (d *. 4.))) r.d)
           s
           r.s2
           (field Bool.to_string r.flag)
           (field (fun t -> Int.to_string (Time_ns.to_int_ns_since_epoch t)) r.ts)
           (field Int.to_string r.px)
           r.n))
;;

let write ?row_group_rows ?compression file rows =
  Writer.with_file ~schema ?row_group_rows ?compression file ~f:(fun w ->
    Writer.add_all w rows)
;;

let%expect_test "random rows with nulls and dictionaries read back identically" =
  let gen =
    Quickcheck.Generator.tuple3
      (List.gen_with_length 60 gen_row
       |> Quickcheck.Generator.bind ~f:(fun rows ->
         Quickcheck.Generator.map (Int.gen_incl 0 60) ~f:(fun n -> List.take rows n)))
      (Int.gen_incl 1 25)
      Bool.quickcheck_generator
  in
  Quickcheck.test
    gen
    ~trials:25
    ~sexp_of:[%sexp_of: row list * int * bool]
    ~f:(fun (rows, row_group_rows, zstd) ->
      Duckdb.with_parquet_file (fun file ->
        write ~row_group_rows ~compression:(if zstd then `Zstd else `None) file rows;
        [%test_result: string] (duckdb_csv file) ~expect:(expected_csv rows)));
  [%expect {| |}]
;;

let row i =
  { a = Option.some_if (i % 3 <> 0) i
  ; b = Option.some_if (i % 2 = 0) (i % 4)
  ; d = None
  ; s = Option.some_if (i % 5 <> 0) (if i % 2 = 0 then "BTC" else "ETH")
  ; s2 = "pq"
  ; flag = Option.some_if (i % 4 = 0) (i % 8 = 0)
  ; ts = None
  ; px = Some i
  ; n = i % 3
  }
;;

let%expect_test "encodings, null counts and statistics as seen by duckdb" =
  Duckdb.with_parquet_file (fun file ->
    write file (List.init 20 ~f:row);
    Duckdb.query
      ~file
      "select path_in_schema, encodings, dictionary_page_offset is not null as dict, \
       stats_null_count, stats_min_value, stats_max_value from parquet_metadata($file)");
  [%expect
    {|
    path_in_schema,encodings,dict,stats_null_count,stats_min_value,stats_max_value
    a,"PLAIN, RLE",false,7,1,19
    b,"PLAIN_DICTIONARY, RLE",true,10,0,2
    d,"PLAIN, RLE",false,20,NULL,NULL
    s,"PLAIN_DICTIONARY, RLE",true,4,BTC,ETH
    s2,PLAIN_DICTIONARY,true,0,pq,pq
    flag,"PLAIN, RLE",false,15,NULL,NULL
    ts,"PLAIN, RLE",false,20,NULL,NULL
    px,"PLAIN, RLE",false,0,0.00,0.19
    n,PLAIN_DICTIONARY,true,0,0,2
    |}]
;;

let%expect_test "a dictionary over 1 MiB falls back to PLAIN" =
  let schema =
    Schema.create
      Schema.Column.
        [ dictionary
            (utf8 "s" (fun (i : int) ->
               String.make 4096 (Char.of_int_exn (97 + (i % 26))) ^ Int.to_string i))
        ]
  in
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema ~row_group_rows:600 file ~f:(fun w ->
      for i = 0 to 699 do
        Writer.add w i
      done);
    Duckdb.query
      ~file
      "select row_group_id, encodings, dictionary_page_offset is not null as dict from \
       parquet_metadata($file)";
    Duckdb.query ~file "select count(*), count(distinct s), sum(length(s)) from $file");
  [%expect
    {|
    row_group_id,encodings,dict
    0,PLAIN,false
    1,PLAIN_DICTIONARY,true
    count_star(),count(DISTINCT s),sum(length(s))
    700,700,2869190
    |}]
;;

let%expect_test "Direct nulls" =
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      let a = Writer.Direct.int w "a"
      and b = Writer.Direct.int w "b"
      and d = Writer.Direct.float w "d"
      and s = Writer.Direct.string w "s"
      and s2 = Writer.Direct.string w "s2"
      and flag = Writer.Direct.bool w "flag"
      and ts = Writer.Direct.int w "ts"
      and px = Writer.Direct.int w "px"
      and n = Writer.Direct.int w "n" in
      for i = 0 to 2 do
        if i = 1
        then (
          Writer.Direct.add_null_int a;
          Writer.Direct.add_null_int b;
          Writer.Direct.add_null_float d;
          Writer.Direct.add_null_string s;
          Writer.Direct.add_null_bool flag;
          Writer.Direct.add_null_int ts;
          Writer.Direct.add_null_int px)
        else (
          Writer.Direct.add_int a i;
          Writer.Direct.add_int b i;
          Writer.Direct.add_float d 0.25;
          Writer.Direct.add_string s "x";
          Writer.Direct.add_bool flag true;
          Writer.Direct.add_int ts 5;
          Writer.Direct.add_int px 125);
        Writer.Direct.add_string s2 "pq";
        Writer.Direct.add_int n i;
        Writer.Direct.end_row w
      done;
      print_s
        [%sexp
          (Or_error.try_with (fun () -> Writer.Direct.add_null_int n) : unit Or_error.t)]);
    print_string (duckdb_csv file));
  [%expect
    {|
    (Error "Parquet.Writer: null in a column that is not optional")
    a,b,d4,s,s2,flag,ts,px,n
    0,0,1,x,pq,true,5,125,0
    NULL,NULL,NULL,NULL,pq,NULL,NULL,NULL,1
    2,2,1,x,pq,true,5,125,2
    |}]
;;
