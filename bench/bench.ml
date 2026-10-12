(* Write trade-like rows and report throughput. Not part of [dune test]:

   {v dune exec bench/bench.exe -- [ROWS] v}

   If [duckdb] is on [PATH] the file is checked with a row count query. *)

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

let run ~rows ~compression path =
  let start = Time_ns.now () in
  Writer.with_file ~schema ~compression path ~f:(fun w ->
    for i = 0 to rows - 1 do
      Writer.add w (trade i)
    done);
  let elapsed = Time_ns.diff (Time_ns.now ()) start |> Time_ns.Span.to_sec in
  let bytes = (Core_unix.stat path).st_size |> Int64.to_int_exn in
  printf
    "%-5s %9d rows  %7.3f s  %6.2f M rows/s  %7.1f MB file\n%!"
    (match compression with
     | `None -> "none"
     | `Zstd -> "zstd")
    rows
    elapsed
    (Float.of_int rows /. elapsed /. 1e6)
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
  List.iter [ `None; `Zstd ] ~f:(fun compression ->
    let path = dir ^/ "bench.parquet" in
    run ~rows ~compression path;
    Sys_unix.remove path);
  Core_unix.rmdir dir
;;
