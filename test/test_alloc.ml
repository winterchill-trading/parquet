open! Core
open Parquet

(* Minor words per [Writer.add] in steady state: no row group flush inside the measured
   window, and the column buffers already grown past it. *)

type row =
  { ts : int
  ; coin : string
  ; px : int
  ; sz : float
  ; is_buy : bool
  }

let schema =
  Schema.create
    Schema.Column.
      [ int64 "ts" (fun r -> r.ts)
      ; utf8 "coin" (fun r -> r.coin)
      ; decimal ~precision:18 ~scale:6 "px" (fun r -> r.px)
      ; float "sz" (fun r -> r.sz)
      ; bool "is_buy" (fun r -> r.is_buy)
      ]
;;

let row = { ts = 1_700_000_000; coin = "BTC"; px = 60_000_000_000; sz = 0.25; is_buy = true }
let n = 1_000

let minor_words f =
  let before = Gc.minor_words () in
  f ();
  Gc.minor_words () - before
;;

let%expect_test "words per Writer.add" =
  Duckdb.with_parquet_file (fun file ->
    Writer.with_file ~schema file ~f:(fun w ->
      for _ = 1 to 100_000 do
        Writer.add w row
      done;
      let words =
        minor_words (fun () ->
          for _ = 1 to n do
            Writer.add w row
          done)
      in
      printf "%d words for %d rows\n" words n));
  [%expect {| 0 words for 1000 rows |}]
;;

(* All words, including the major heap where column buffers grow and pages are copied,
   for a whole file of one row group. *)
let total_words f =
  let s0 = Gc.quick_stat () in
  f ();
  let s1 = Gc.quick_stat () in
  Float.to_int
    (s1.minor_words
     -. s0.minor_words
     +. (s1.major_words -. s0.major_words)
     -. (s1.promoted_words -. s0.promoted_words))
;;

let%expect_test "words per file of 100_000 rows" =
  List.iter [ `None; `Zstd ] ~f:(fun compression ->
    Duckdb.with_parquet_file (fun file ->
      let words =
        total_words (fun () ->
          Writer.with_file ~schema ~compression file ~f:(fun w ->
            for _ = 1 to 100_000 do
              Writer.add w row
            done))
      in
      printf "%.2f words per row\n" (Float.of_int words /. 100_000.)));
  [%expect {|
    15.46 words per row
    13.45 words per row
    |}]
;;
