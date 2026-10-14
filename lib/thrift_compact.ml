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

let[@cold] malformed what ~pos =
  raise_s [%message "Thrift_compact: malformed input" (what : string) (pos : int)]
;;

let max_depth = 64

let read_struct s ~pos =
  let p = ref pos in
  let len = String.length s in
  let byte () =
    if !p >= len then malformed "truncated" ~pos:!p;
    let b = Char.to_int (String.unsafe_get s !p) in
    incr p;
    b
  in
  let uvarint () =
    let rec go shift acc =
      if shift > 63 then malformed "varint too long" ~pos:!p;
      let b = byte () in
      let low = Int64.of_int (b land 0x7F) in
      let acc = Int64.bit_or acc (Int64.shift_left low shift) in
      if b land 0x80 = 0 then acc else go (shift + 7) acc
    in
    go 0 0L
  in
  let svarint () =
    let n = uvarint () in
    Int64.bit_xor (Int64.shift_right_logical n 1) (Int64.neg (Int64.bit_and n 1L))
  in
  let small_int ~bits what =
    let n = svarint () in
    let limit = Int64.shift_left 1L (bits - 1) in
    if Int64.( < ) n (Int64.neg limit) || Int64.( >= ) n limit then malformed what ~pos:!p;
    Int64.to_int_exn n
  in
  let type_of_id id : Type.t =
    match id with
    | 1 | 2 -> Bool
    | 3 -> Byte
    | 4 -> I16
    | 5 -> I32
    | 6 -> I64
    | 7 -> Double
    | 8 -> Binary
    | 9 | 10 -> List
    | 12 -> Struct
    | _ -> malformed (sprintf "unsupported type %d" id) ~pos:!p
  in
  let rec value (ty : Type.t) ~depth =
    if depth > max_depth then malformed "nested too deep" ~pos:!p;
    match ty with
    | Bool -> Bool (byte () = 1)
    | Byte -> Byte (byte ())
    | I16 -> I16 (small_int ~bits:16 "i16 out of range")
    | I32 -> I32 (Int32.of_int_exn (small_int ~bits:32 "i32 out of range"))
    | I64 -> I64 (svarint ())
    | Double ->
      let bits = ref 0L in
      for i = 0 to 7 do
        let b = Int64.of_int (byte ()) in
        bits := Int64.bit_or !bits (Int64.shift_left b (8 * i))
      done;
      Double (Int64.float_of_bits !bits)
    | Binary ->
      let n = uvarint () in
      if Int64.( < ) n 0L || Int64.( > ) n (Int64.of_int (len - !p))
      then malformed "binary length" ~pos:!p;
      let n = Int64.to_int_exn n in
      let s = String.sub s ~pos:!p ~len:n in
      p := !p + n;
      Binary s
    | List ->
      let header = byte () in
      let elem_type = type_of_id (header land 0x0F) in
      let size =
        if header lsr 4 = 15
        then (
          let n = uvarint () in
          (* Every element takes at least one byte. *)
          if Int64.( < ) n 0L || Int64.( > ) n (Int64.of_int (len - !p))
          then malformed "list size" ~pos:!p;
          Int64.to_int_exn n)
        else header lsr 4
      in
      (* Not [List.init]: it may call [f] in any order, and reading is sequential. *)
      let elems = ref [] in
      for _ = 1 to size do
        elems := value elem_type ~depth:(depth + 1) :: !elems
      done;
      List (elem_type, List.rev !elems)
    | Struct -> struct_ ~depth
  and struct_ ~depth =
    let rec fields last_id acc =
      let header = byte () in
      if header = 0
      then Struct (List.rev acc)
      else (
        let type_id = header land 0x0F in
        let delta = header lsr 4 in
        let id = if delta = 0 then small_int ~bits:16 "field id" else last_id + delta in
        let v =
          match type_id with
          | 1 -> Bool true
          | 2 -> Bool false
          | type_id -> value (type_of_id type_id) ~depth:(depth + 1)
        in
        fields id ((id, v) :: acc))
    in
    fields 0 []
  in
  let v = struct_ ~depth:0 in
  v, !p
;;

let of_string s =
  let v, stop = read_struct s ~pos:0 in
  if stop <> String.length s then malformed "trailing bytes" ~pos:stop;
  v
;;
