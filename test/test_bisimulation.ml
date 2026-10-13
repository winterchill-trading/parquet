open! Core
open Parquet

(* Files written by [Writer] must be byte-for-byte the files [Reference_writer] (the
   original implementation) writes, for every column type, row group size and codec. *)

type row =
  { i32 : int
  ; i64 : int
  ; d : float
  ; s : string
  ; raw : string
  ; ts : Time_ns.t
  ; dec : int
  ; flag : bool
  }
[@@deriving sexp_of]

let gen_row =
  let open Quickcheck.Generator.Let_syntax in
  let%map i32 = Int.gen_incl (-0x8000_0000) 0x7FFF_FFFF
  and i64 = Int.quickcheck_generator
  and d =
    Quickcheck.Generator.union
      [ Float.quickcheck_generator; Quickcheck.Generator.of_list [ 0.; -0.; Float.nan ] ]
  and s = String.gen_with_length 3 Char.gen_alphanum
  and raw = String.quickcheck_generator
  and ts = Int.gen_incl 0 Int.max_value >>| Time_ns.of_int_ns_since_epoch
  and dec = Int.gen_incl (-999_999_999) 999_999_999
  and flag = Bool.quickcheck_generator in
  { i32; i64; d; s; raw; ts; dec; flag }
;;

let schema =
  Schema.create
    Schema.Column.
      [ int32 "i32" (fun r -> r.i32)
      ; int64 "i64" (fun r -> r.i64)
      ; float "d" (fun r -> r.d)
      ; utf8 "s" (fun r -> r.s)
      ; bytes "raw" (fun r -> r.raw)
      ; timestamp_ns "ts" (fun r -> r.ts)
      ; decimal ~precision:9 ~scale:2 "dec" (fun r -> r.dec)
      ; bool "flag" (fun r -> r.flag)
      ]
;;

let write_both rows ~row_group_rows ~compression =
  Duckdb.with_parquet_file (fun file ->
    let reference = file ^ ".reference" in
    Writer.with_file ~schema ~row_group_rows ~compression file ~f:(fun w ->
      Writer.add_all w rows);
    Reference_writer.with_file ~schema ~row_group_rows ~compression reference ~f:(fun w ->
      Reference_writer.add_all w rows);
    In_channel.read_all file, In_channel.read_all reference)
;;

let%expect_test "Writer and Reference_writer write identical bytes" =
  let gen =
    let open Quickcheck.Generator.Let_syntax in
    let%map rows = Int.gen_incl 0 200 >>= fun n -> List.gen_with_length n gen_row
    and row_group_rows = Int.gen_incl 1 64
    and compression = Quickcheck.Generator.of_list [ `None; `Zstd ] in
    rows, row_group_rows, compression
  in
  Quickcheck.test
    gen
    ~trials:100
    ~sexp_of:[%sexp_of: row list * int * [ `None | `Zstd ]]
    ~f:(fun (rows, row_group_rows, compression) ->
      let actual, expect = write_both rows ~row_group_rows ~compression in
      [%test_result: string] actual ~expect);
  [%expect {| |}]
;;
