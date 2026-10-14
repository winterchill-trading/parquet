(* Write a few trades to a Parquet file, then read them back.

   {v dune exec examples/trades.exe -- trades.parquet v} *)

open! Core

type trade =
  { ts : Time_ns.t
  ; coin : string
  ; px : int (* unscaled, 6 decimals *)
  ; sz : float
  ; is_buy : bool
  ; fee : int option (* unscaled, 6 decimals; unknown for some trades *)
  }

let schema =
  Parquet.Schema.create
    Parquet.Schema.Column.
      [ timestamp_ns "ts" (fun t -> t.ts)
      ; dictionary (utf8 "coin" (fun t -> t.coin))
      ; decimal ~precision:18 ~scale:6 "px" (fun t -> t.px)
      ; float "sz" (fun t -> t.sz)
      ; bool "is_buy" (fun t -> t.is_buy)
      ; optional (decimal ~precision:18 ~scale:6) "fee" (fun t -> t.fee)
      ]
;;

let trades =
  List.init 1_000 ~f:(fun i ->
    { ts = Time_ns.of_int_ns_since_epoch (1_700_000_000_000_000_000 + (i * 1_000_000))
    ; coin = (if i % 3 = 0 then "ETH" else "BTC")
    ; px = 60_000_000_000 + (i * 1_000_000)
    ; sz = Float.of_int (i % 10) /. 4.
    ; is_buy = i % 2 = 0
    ; fee = Option.some_if (i % 5 <> 0) (i * 10)
    })
;;

let () =
  let path =
    match Sys.get_argv () with
    | [| _; path |] -> path
    | _ -> "trades.parquet"
  in
  Parquet.Writer.with_file
    ~schema
    ~compression:`Zstd
    ~key_value_metadata:[ "source", "examples/trades.ml" ]
    path
    ~f:(fun w -> List.iter trades ~f:(Parquet.Writer.add w));
  let rows, nulls =
    Parquet.Reader.with_file path ~f:(fun r ->
      Parquet.Reader.fold_rows r ~init:(0, 0) ~f:(fun (rows, nulls) row ->
        let fee = row.(5) in
        rows + 1, if Parquet.Reader.Value.equal fee Null then nulls + 1 else nulls))
    |> Or_error.ok_exn
  in
  printf "%s: %d rows, %d without a fee\n" path rows nulls
;;
