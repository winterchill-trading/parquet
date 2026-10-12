open! Core

module Type = struct
  type t =
    | Bool
    | Byte
    | I16
    | I32
    | I64
    | Double
    | Binary
    | List
    | Struct
  [@@deriving sexp_of, equal]

  (* Compact protocol type ids. Booleans inside a field header carry their value in the
     type nibble (1 = true, 2 = false); in list headers the element type is 1. *)
  let to_int = function
    | Bool -> 1
    | Byte -> 3
    | I16 -> 4
    | I32 -> 5
    | I64 -> 6
    | Double -> 7
    | Binary -> 8
    | List -> 9
    | Struct -> 12
  ;;
end

type t =
  | Bool of bool
  | Byte of int
  | I16 of int
  | I32 of int32
  | I64 of int64
  | Double of float
  | Binary of string
  | List of Type.t * t list
  | Struct of (int * t) list
[@@deriving sexp_of]

let type_of : t -> Type.t = function
  | Bool _ -> Bool
  | Byte _ -> Byte
  | I16 _ -> I16
  | I32 _ -> I32
  | I64 _ -> I64
  | Double _ -> Double
  | Binary _ -> Binary
  | List _ -> List
  | Struct _ -> Struct
;;

let i32 n = I32 (Int32.of_int_exn n)
let i64 n = I64 (Int64.of_int n)
let binary s = Binary s
let zigzag64 n = Int64.bit_xor (Int64.shift_left n 1) (Int64.shift_right n 63)

let rec write_uvarint buf n =
  let low = Int64.to_int_exn (Int64.bit_and n 0x7FL) in
  let rest = Int64.shift_right_logical n 7 in
  if Int64.equal rest 0L
  then Buffer.add_char buf (Char.of_int_exn low)
  else (
    Buffer.add_char buf (Char.of_int_exn (low lor 0x80));
    write_uvarint buf rest)
;;

let write_svarint buf n = write_uvarint buf (zigzag64 n)

let write_double buf x =
  let bits = Int64.bits_of_float x in
  for i = 0 to 7 do
    let shift = 8 * i in
    let byte = Int64.(to_int_exn (bit_and (shift_right_logical bits shift) 0xFFL)) in
    Buffer.add_char buf (Char.of_int_exn byte)
  done
;;

let write_list_header buf elem_type size =
  let ty = Type.to_int elem_type in
  if size < 15
  then Buffer.add_char buf (Char.of_int_exn ((size lsl 4) lor ty))
  else (
    Buffer.add_char buf (Char.of_int_exn (0xF0 lor ty));
    write_uvarint buf (Int64.of_int size))
;;

let field_type_nibble value =
  match value with
  | Bool true -> 1
  | Bool false -> 2
  | v -> Type.to_int (type_of v)
;;

let rec write buf value =
  match value with
  | Bool b -> Buffer.add_char buf (if b then '\001' else '\000')
  | Byte n -> Buffer.add_char buf (Char.of_int_exn (n land 0xFF))
  | I16 n -> write_svarint buf (Int64.of_int n)
  | I32 n -> write_svarint buf (Int64.of_int32 n)
  | I64 n -> write_svarint buf n
  | Double x -> write_double buf x
  | Binary s ->
    write_uvarint buf (Int64.of_int (String.length s));
    Buffer.add_string buf s
  | List (elem_type, elems) ->
    write_list_header buf elem_type (List.length elems);
    List.iter elems ~f:(fun elem ->
      if not (Type.equal (type_of elem) elem_type)
      then
        raise_s
          [%message
            "Thrift_compact: list element type mismatch" (elem_type : Type.t) (elem : t)];
      write buf elem)
  | Struct fields -> write_struct buf fields

and write_struct buf fields =
  let last_id =
    List.fold fields ~init:0 ~f:(fun last_id (id, value) ->
      if id <= last_id
      then raise_s [%message "Thrift_compact: field ids must increase" (id : int)];
      let delta = id - last_id in
      let nibble = field_type_nibble value in
      if delta <= 15
      then Buffer.add_char buf (Char.of_int_exn ((delta lsl 4) lor nibble))
      else (
        Buffer.add_char buf (Char.of_int_exn nibble);
        write_svarint buf (Int64.of_int id));
      (match value with
       | Bool _ -> ()
       | v -> write buf v);
      id)
  in
  ignore (last_id : int);
  Buffer.add_char buf '\000'
;;

let to_string value =
  let buf = Buffer.create 64 in
  write buf value;
  Buffer.contents buf
;;
