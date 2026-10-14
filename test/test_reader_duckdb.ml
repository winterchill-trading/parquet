open! Core
open Parquet
module V = Reader.Value

(* Files written by DuckDB (dictionary pages, definition levels on every column, several
   row groups, INT32 decimals, FLOAT, UUID as FIXED_LEN_BYTE_ARRAY) read by [Reader] must
   print exactly what DuckDB prints for the same file. *)

let source =
  "select (i * 7919) % 100003 - 50000 as big, case when i % 7 = 0 then null else (i % \
   1000)::integer end as small, case when i % 5 = 0 then null when i % 11 = 0 then '' \
   else 'c' || (i % 13) end as s, case when i % 3 = 0 then null else i % 2 = 0 end as b, \
   (i % 1000)::double / 4 as d, case when i % 9 = 0 then null else ((i % 100)::real / 2) \
   end as r, make_timestamp_ns(1700000000000000000 + i * 1001) as ts, ((i % 100000) / \
   100)::decimal(9, 2) as dec, case when i % 17 = 0 then null else lpad(to_hex(i), 32, \
   '0')::uuid end as u from range(50000) t(i)"
;;

(* DuckDB's own rendering of the file, through casts that make every value an integer,
   string or boolean. *)
let duckdb_csv file =
  Duckdb.with_output (fun () ->
    Duckdb.query
      ~file
      "select big, small, s, b, (d * 4)::bigint as d, (r * 2)::bigint as r, epoch_ns(ts) \
       as ts, (dec * 100)::bigint as dec, replace(u::varchar, '-', '') as u from \
       read_parquet($file, file_row_number = true) order by file_row_number")
;;

let reader_csv file =
  let render (v : V.t) ~scale =
    match v with
    | Null -> "NULL"
    | Int n -> Int.to_string n
    | Float x -> Int.to_string (Float.to_int (x *. scale))
    | Bool b -> Bool.to_string b
    | String s -> s
  in
  let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c)) in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "big,small,s,b,d,r,ts,dec,u\n";
  Reader.with_file file ~f:(fun r ->
    Reader.fold_rows r ~init:() ~f:(fun () row ->
      let cells =
        Array.to_list
          (Array.mapi row ~f:(fun i v ->
             match i, v with
             | 4, _ -> render v ~scale:4.
             | 5, _ -> render v ~scale:2.
             | 8, String u -> hex u
             | _ -> render v ~scale:1.))
      in
      Buffer.add_string buf (String.concat ~sep:"," cells);
      Buffer.add_char buf '\n'))
  |> Or_error.ok_exn;
  Buffer.contents buf
;;

let copy file options =
  let out =
    Duckdb.with_output (fun () ->
      Duckdb.query ~file (sprintf "copy (%s) to $file (format parquet%s)" source options))
  in
  if not (String.is_empty out) then print_string out
;;

let%expect_test "files written by duckdb" =
  List.iter
    [ ", compression uncompressed"
    ; ", compression zstd"
    ; ", compression zstd, row_group_size 7000"
    ]
    ~f:(fun options ->
      Duckdb.with_parquet_file (fun file ->
        copy file options;
        let expected = duckdb_csv file in
        let got = reader_csv file in
        printf
          "%s: %d lines, equal %b\n"
          options
          (List.length (String.split_lines got))
          (String.equal expected got);
        List.zip_exn (String.split_lines expected) (String.split_lines got)
        |> List.filter ~f:(fun (e, g) -> not (String.equal e g))
        |> (fun l -> List.take l 3)
        |> List.iter ~f:(fun (e, g) -> printf "duckdb: %s\nreader: %s\n" e g)));
  [%expect
    {|
    , compression uncompressed: 50001 lines, equal true
    , compression zstd: 50001 lines, equal true
    , compression zstd, row_group_size 7000: 50001 lines, equal true
    |}]
;;

let%expect_test "what the duckdb files contain" =
  Duckdb.with_parquet_file (fun file ->
    copy file ", compression zstd";
    Reader.with_file file ~f:(fun r ->
      let meta = Reader.metadata r in
      List.iter (List.hd_exn meta.row_groups).columns ~f:(fun c ->
        print_s
          [%sexp
            (c.meta_data.path_in_schema : string list)
            , (c.meta_data.type_ : Metadata.Physical_type.t)
            , (c.meta_data.encodings : Metadata.Encoding.t list)]);
      Ok ())
    |> Or_error.ok_exn);
  [%expect
    {|
    ((big) Int64 (Plain))
    ((small) Int32 (Plain_dictionary))
    ((s) Byte_array (Plain_dictionary))
    ((b) Boolean (Plain))
    ((d) Double (Plain_dictionary))
    ((r) Float (Plain_dictionary))
    ((ts) Int64 (Plain))
    ((dec) Int32 (Plain))
    ((u) Fixed_len_byte_array (Plain))
    |}]
;;

let%expect_test "unsupported codecs are reported" =
  Duckdb.with_parquet_file (fun file ->
    copy file ", compression snappy";
    Reader.with_file file ~f:(fun r -> Reader.read_column r ~row_group:0 "big")
    |> Result.map ~f:(fun _ -> ())
    |> [%sexp_of: unit Or_error.t]
    |> Sexp.to_string_hum
    |> String.substr_replace_all ~pattern:file ~with_:"$file"
    |> print_endline);
  [%expect
    {|
    (Error
     ($file
      ("Parquet.Reader: unsupported compression codec" (codec Snappy))))
    |}]
;;
