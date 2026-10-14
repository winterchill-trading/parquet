open! Core
open Parquet
module V = Reader.Value

(* Files written by [Writer] read back to the values that went in, for every column kind,
   with nulls, dictionaries, row group sizes and codecs. *)

type row =
  { i32 : int option
  ; i64 : int
  ; d : float
  ; s : string option
  ; raw : string
  ; ts : Time_ns.t
  ; dec : int option
  ; flag : bool
  ; oflag : bool option
  ; sym : string
  }
[@@deriving sexp_of]

let schema =
  Schema.create
    Schema.Column.
      [ optional int32 "i32" (fun r -> r.i32)
      ; dictionary (int64 "i64" (fun r -> r.i64))
      ; float "d" (fun r -> r.d)
      ; dictionary (optional utf8 "s" (fun r -> r.s))
      ; bytes "raw" (fun r -> r.raw)
      ; timestamp_ns "ts" (fun r -> r.ts)
      ; optional (decimal ~precision:18 ~scale:4) "dec" (fun r -> r.dec)
      ; bool "flag" (fun r -> r.flag)
      ; optional bool "oflag" (fun r -> r.oflag)
      ; dictionary (utf8 "sym" (fun r -> r.sym))
      ]
;;

let gen_row =
  let open Quickcheck.Generator.Let_syntax in
  let opt g =
    Quickcheck.Generator.weighted_union [ 1., return None; 3., g >>| Option.some ]
  in
  let%map i32 = opt (Int.gen_incl (-0x8000_0000) 0x7FFF_FFFF)
  and i64 = Quickcheck.Generator.of_list [ 0; 1; -1; Int.max_value; Int.min_value ]
  and d =
    Quickcheck.Generator.union
      [ Float.quickcheck_generator; Quickcheck.Generator.of_list [ 0.; -0.; Float.nan ] ]
  and s = opt (String.gen_with_length 2 Char.gen_alphanum)
  and raw = String.quickcheck_generator
  and ts = Int.gen_incl 0 Int.max_value >>| Time_ns.of_int_ns_since_epoch
  and dec = opt Int.quickcheck_generator
  and flag = Bool.quickcheck_generator
  and oflag = opt Bool.quickcheck_generator
  and sym = Quickcheck.Generator.of_list [ "BTC"; "ETH" ] in
  { i32; i64; d; s; raw; ts; dec; flag; oflag; sym }
;;

let values r : V.t array =
  let opt f = function
    | None -> V.Null
    | Some v -> f v
  in
  [| opt (fun v -> V.Int v) r.i32
   ; Int r.i64
   ; Float r.d
   ; opt (fun v -> V.String v) r.s
   ; String r.raw
   ; Int (Time_ns.to_int_ns_since_epoch r.ts)
   ; opt (fun v -> V.Int v) r.dec
   ; Bool r.flag
   ; opt (fun v -> V.Bool v) r.oflag
   ; String r.sym
  |]
;;

let read_all file =
  Reader.with_file file ~f:(fun r ->
    Reader.fold_rows r ~init:[] ~f:(fun acc row -> row :: acc) |> Or_error.map ~f:List.rev)
  |> Or_error.ok_exn
;;

let%test_unit "Writer files read back" =
  Quickcheck.test
    (Quickcheck.Generator.tuple3
       (List.gen_with_length 40 gen_row
        |> Quickcheck.Generator.bind ~f:(fun rows ->
          Quickcheck.Generator.map (Int.gen_incl 0 40) ~f:(fun n -> List.take rows n)))
       (Int.gen_incl 1 15)
       Bool.quickcheck_generator)
    ~trials:50
    ~sexp_of:[%sexp_of: row list * int * bool]
    ~f:(fun (rows, row_group_rows, zstd) ->
      Duckdb.with_parquet_file (fun file ->
        Writer.with_file
          ~schema
          ~row_group_rows
          ~compression:(if zstd then `Zstd else `None)
          file
          ~f:(fun w -> Writer.add_all w rows);
        (* Compare through sexps so that NaN equals NaN. *)
        [%test_result: Sexp.t]
          [%sexp (read_all file : V.t array list)]
          ~expect:[%sexp (List.map rows ~f:values : V.t array list)]))
;;

let%expect_test "metadata of a written file" =
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file
      ~schema
      ~row_group_rows:2
      ~key_value_metadata:[ "origin", "test" ]
      file
      ~f:(fun w ->
        Writer.add_all
          w
          (List.init 3 ~f:(fun i ->
             { i32 = Some i
             ; i64 = i
             ; d = 0.5
             ; s = None
             ; raw = ""
             ; ts = Time_ns.epoch
             ; dec = None
             ; flag = true
             ; oflag = None
             ; sym = "BTC"
             })));
    Reader.with_file file ~f:(fun r ->
      print_s
        [%sexp
          { num_rows = (Reader.num_rows r : int)
          ; row_groups = (Reader.num_row_groups r : int)
          ; columns =
              (List.map (Reader.columns r) ~f:(fun c -> c.name, c.repetition_type)
               : (string * Metadata.Repetition.t option) list)
          ; kv = ((Reader.metadata r).key_value_metadata : Metadata.Key_value.t list)
          }];
      Ok ())
    |> Or_error.ok_exn;
    Duckdb.query ~file "select key, value from parquet_kv_metadata($file)");
  [%expect
    {|
    ((num_rows 3) (row_groups 2)
     (columns
      ((i32 (Optional)) (i64 (Required)) (d (Required)) (s (Optional))
       (raw (Required)) (ts (Required)) (dec (Optional)) (flag (Required))
       (oflag (Optional)) (sym (Required))))
     (kv (((key origin) (value (test))))))
    key,value
    origin,test
    |}]
;;

let%expect_test "errors" =
  Duckdb.with_parquet_file (fun file ->
    let show r =
      (* The temporary path changes on every run. *)
      Result.map r ~f:(fun _ -> ())
      |> [%sexp_of: unit Or_error.t]
      |> Sexp.to_string_hum
      |> String.substr_replace_all ~pattern:file ~with_:"$file"
      |> print_endline
    in
    Out_channel.write_all file ~data:"PAR1 not a parquet file PAR1";
    show (Reader.create file);
    Out_channel.write_all file ~data:"hello";
    show (Reader.create file);
    Writer.with_file ~schema file ~f:(fun _ -> ());
    Reader.with_file file ~f:(fun r ->
      show (Reader.read_column r ~row_group:0 "i32");
      show (Reader.read_column r ~row_group:0 "nope");
      Ok ())
    |> Or_error.ok_exn);
  [%expect
    {|
    (Error
     ($file
      "Parquet.Reader: bad footer length"))
    (Error
     ($file
      "Parquet.Reader: file too short"))
    (Error
     ($file
      ("Parquet.Reader: no such row group" (i 0))))
    (Error
     ($file
      ("Parquet.Reader: no such column" nope)))
    |}]
;;
