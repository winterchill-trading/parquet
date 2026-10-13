open! Core
open Parquet

let compress_bytes s =
  let len = String.length s in
  let src = Bytes.of_string (s ^ "trailing bytes past len are ignored") in
  let dst = Bytes.create (Zstd_bytes.compress_bound len) in
  let n = Zstd_bytes.compress ~level:3 src ~len ~dst in
  Bytes.To_string.sub dst ~pos:0 ~len:n
;;

let%test_unit "same frames as Zstd.compress" =
  Quickcheck.test
    (Quickcheck.Generator.union
       [ String.quickcheck_generator
       ; Int.gen_incl 0 100_000 |> Quickcheck.Generator.map ~f:(fun n -> String.make n 'a')
       ])
    ~trials:200
    ~f:(fun s ->
      [%test_result: string] (compress_bytes s) ~expect:(Zstd.compress ~level:3 s))
;;

let%expect_test "a destination that is too small raises" =
  let src = Bytes.of_string (String.init 1000 ~f:(fun i -> Char.of_int_exn (i * 7919 % 256))) in
  print_s
    [%sexp
      (Result.try_with (fun () ->
         Zstd_bytes.compress ~level:3 src ~len:1000 ~dst:(Bytes.create 10))
       : (int, exn) Result.t)];
  [%expect {| (Error (Failure "Destination buffer is too small")) |}]
;;
