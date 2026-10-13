(* Write trade-like rows and report throughput. Not part of [dune test]:

   {v dune exec bench/bench.exe -- [ROWS] v}

   Each row goes once through [Writer.add] (a record per row, schema accessors) and once
   through [Writer.Direct]. If [duckdb] is on [PATH] the file is checked with a row count
   query. *)

open! Core
open Parquet

type trade =
  { ts : Time_ns.t
  ; coin : string
  ; px : int
  ; sz : float
  ; is_buy : bool
  ; trade_id : int
  }

let schema =
  Schema.create
    Schema.Column.
      [ timestamp_ns "ts" (fun t -> t.ts)
      ; utf8 "coin" (fun t -> t.coin)
      ; decimal ~precision:18 ~scale:6 "px" (fun t -> t.px)
      ; float "sz" (fun t -> t.sz)
      ; bool "is_buy" (fun t -> t.is_buy)
      ; int64 "trade_id" (fun t -> t.trade_id)
      ]
;;

let coins = [| "BTC"; "ETH"; "SOL"; "HYPE"; "DOGE" |]

let trade i =
  { ts = Time_ns.of_int_ns_since_epoch (1_700_000_000_000_000_000 + (i * 1_000_123))
  ; coin = coins.(i % Array.length coins)
  ; px = 60_000_000_000 + (i * 7919 % 1_000_000)
  ; sz = Float.of_int (i % 1000) /. 100.
  ; is_buy = i land 1 = 0
  ; trade_id = i
  }
;;

(* Words allocated on the minor and major heaps, excluding promotions. *)
let words () =
  let s = Gc.quick_stat () in
  s.minor_words +. s.major_words -. s.promoted_words
;;

external unbox_float : float -> float# = "%unbox_float"

(* The same rows through [Writer.Direct], without building a record per row. *)
let add_direct w ~rows =
  let open Writer.Direct in
  let ts = int w "ts"
  and coin = string w "coin"
  and px = int w "px"
  and sz = float w "sz"
  and is_buy = bool w "is_buy"
  and trade_id = int w "trade_id" in
  for i = 0 to rows - 1 do
    add_int ts (1_700_000_000_000_000_000 + (i * 1_000_123));
    add_string coin coins.(i % Array.length coins);
    add_int px (60_000_000_000 + (i * 7919 % 1_000_000));
    add_float_u sz (unbox_float (Float.of_int (i % 1000) /. 100.));
    add_bool is_buy (i land 1 = 0);
    add_int trade_id i;
    end_row w
  done
;;

let run ~rows ~direct ~compression path =
  let start = Time_ns.now () in
  let words_before = words () in
  Writer.with_file ~schema ~compression path ~f:(fun w ->
    if direct
    then add_direct w ~rows
    else
      for i = 0 to rows - 1 do
        Writer.add w (trade i)
      done);
  let words = words () -. words_before in
  let elapsed = Time_ns.diff (Time_ns.now ()) start |> Time_ns.Span.to_sec in
  let bytes = (Core_unix.stat path).st_size |> Int64.to_int_exn in
  printf
    "%-6s %-5s %9d rows  %7.3f s  %6.2f M rows/s  %6.1f words/row  %7.1f MB file\n%!"
    (if direct then "direct" else "add")
    (match compression with
     | `None -> "none"
     | `Zstd -> "zstd")
    rows
    elapsed
    (Float.of_int rows /. elapsed /. 1e6)
    (words /. Float.of_int rows)
    (Float.of_int bytes /. 1e6);
  match
    Sys_unix.command
      (sprintf "duckdb -csv -noheader -c \"select count(*) from '%s'\"" path)
  with
  | 0 -> ()
  | _ -> print_endline "(duckdb check skipped or failed)"
;;

let () =
  let rows =
    match Sys.get_argv () with
    | [| _; n |] -> Int.of_string n
    | _ -> 1_000_000
  in
  let dir = Filename_unix.temp_dir "parquet" "bench" in
  List.iter [ false; true ] ~f:(fun direct ->
    List.iter [ `None; `Zstd ] ~f:(fun compression ->
      let path = dir ^/ "bench.parquet" in
      run ~rows ~direct ~compression path;
      Sys_unix.remove path));
  Core_unix.rmdir dir
;;
