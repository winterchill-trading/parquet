open! Core
open Parquet

type trade =
  { ts : Time_ns.t
  ; coin : string
  ; px : int
  ; sz : float
  ; is_buy : bool
  }

let schema =
  Schema.create
    Schema.Column.
      [ timestamp_ns "ts" (fun t -> t.ts)
      ; utf8 "coin" (fun t -> t.coin)
      ; decimal ~precision:18 ~scale:4 "px" (fun t -> t.px)
      ; float "sz" (fun t -> t.sz)
      ; bool "is_buy" (fun t -> t.is_buy)
      ]
;;

let%expect_test "schema elements" =
  List.iter (Schema.schema_elements schema) ~f:(fun e ->
    print_s [%sexp (e : Metadata.Schema_element.t)]);
  [%expect
    {|
    ((type_ ()) (type_length ()) (repetition_type ()) (name schema)
     (num_children (5)) (converted_type ()) (scale ()) (precision ())
     (logical_type ()))
    ((type_ (Int64)) (type_length ()) (repetition_type (Required)) (name ts)
     (num_children ()) (converted_type ()) (scale ()) (precision ())
     (logical_type ((Timestamp (is_adjusted_to_utc true) (unit Nanos)))))
    ((type_ (Byte_array)) (type_length ()) (repetition_type (Required))
     (name coin) (num_children ()) (converted_type (Utf8)) (scale ())
     (precision ()) (logical_type (String)))
    ((type_ (Int64)) (type_length ()) (repetition_type (Required)) (name px)
     (num_children ()) (converted_type (Decimal)) (scale (4)) (precision (18))
     (logical_type ((Decimal (scale 4) (precision 18)))))
    ((type_ (Double)) (type_length ()) (repetition_type (Required)) (name sz)
     (num_children ()) (converted_type ()) (scale ()) (precision ())
     (logical_type ()))
    ((type_ (Boolean)) (type_length ()) (repetition_type (Required))
     (name is_buy) (num_children ()) (converted_type ()) (scale ())
     (precision ()) (logical_type ()))
    |}]
;;

let%expect_test "invalid schemas" =
  let try_ f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  try_ (fun () -> ignore (Schema.create ([] : unit Schema.Column.t list)));
  try_ (fun () ->
    ignore (Schema.create Schema.Column.[ int32 "a" Fn.id; int64 "a" Fn.id ]));
  try_ (fun () ->
    ignore (Schema.Column.decimal ~precision:19 ~scale:2 "d" (Fn.id : int -> int)));
  [%expect
    {|
    (Error "Parquet.Schema.create: no columns")
    (Error ("Parquet.Schema.create: duplicate column" (name a)))
    (Error
     ("Parquet.Schema.Column.decimal: unsupported precision or scale" (name d)
      (precision 19) (scale 2)))
    |}]
;;
