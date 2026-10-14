open! Core
module M = Metadata

let magic = "PAR1"
let created_by = "parquet-ocaml version 0.1.0"
let zstd_level = 3

(* Typed column builders for the current row group, with running statistics. Values are
   appended by [Writer.add] through the schema accessors or directly by the caller through
   [Direct]; [count] lets [Direct.end_row] check that every column got one value. *)

module Int_col = struct
  type t =
    { is_32 : bool
    ; buf : Byte_buf.t
    ; mutable min : int
    ; mutable max : int
    ; mutable count : int
    }

  let create ~is_32 =
    { is_32
    ; buf = Byte_buf.create 4096
    ; min = Int.max_value
    ; max = Int.min_value
    ; count = 0
    }
  ;;

  let[@cold] out_of_range n =
    raise_s [%message "Parquet.Encoding.int32: out of range" (n : int)]
  ;;

  let[@inline] add t v =
    if t.is_32
    then (
      if v < -0x8000_0000 || v > 0x7FFF_FFFF then out_of_range v;
      Byte_buf.add_int32_le t.buf v)
    else Byte_buf.add_int64_le t.buf v;
    if v < t.min then t.min <- v;
    if v > t.max then t.max <- v;
    t.count <- t.count + 1
  ;;

  let reset t =
    Byte_buf.clear t.buf;
    t.min <- Int.max_value;
    t.max <- Int.min_value;
    t.count <- 0
  ;;
end

module Double_col = struct
  (* An all-float record keeps [min] and [max] unboxed, so updating them never allocates. *)
  type stats =
    { mutable min : float
    ; mutable max : float
    }

  type t =
    { buf : Byte_buf.t
    ; stats : stats
    ; mutable count : int
    }

  let create () =
    { buf = Byte_buf.create 4096
    ; stats = { min = Float.nan; max = Float.nan }
    ; count = 0
    }
  ;;

  external box_float : float# -> float = "%box_float"
  external unbox_float : float -> float# = "%unbox_float"

  let[@inline] add_u t x =
    Byte_buf.add_double_u_le t.buf x;
    let v = box_float x in
    (* NaN is not ordered and must not appear in statistics. *)
    if not (Float.is_nan v)
    then (
      let s = t.stats in
      if Float.is_nan s.min || Float.( < ) v s.min then s.min <- v;
      if Float.is_nan s.max || Float.( > ) v s.max then s.max <- v);
    t.count <- t.count + 1
  ;;

  let[@inline] add t v = add_u t (unbox_float v)

  let reset t =
    Byte_buf.clear t.buf;
    t.stats.min <- Float.nan;
    t.stats.max <- Float.nan;
    t.count <- 0
  ;;
end

module Bytes_col = struct
  (* [min] and [max] are meaningful only when [count > 0]. *)
  type t =
    { buf : Byte_buf.t
    ; mutable min : string
    ; mutable max : string
    ; mutable count : int
    }

  let create () = { buf = Byte_buf.create 4096; min = ""; max = ""; count = 0 }

  let[@inline] add t v =
    Byte_buf.add_int32_le t.buf (String.length v);
    Byte_buf.add_string t.buf v;
    if t.count = 0
    then (
      t.min <- v;
      t.max <- v)
    else (
      if String.( < ) v t.min then t.min <- v;
      if String.( > ) v t.max then t.max <- v);
    t.count <- t.count + 1
  ;;

  let reset t =
    Byte_buf.clear t.buf;
    t.min <- "";
    t.max <- "";
    t.count <- 0
  ;;
end

module Bool_col = struct
  (* Bit-packed, least significant bit first; [byte] holds the [bits] pending bits. *)
  type t =
    { buf : Byte_buf.t
    ; mutable byte : int
    ; mutable bits : int
    ; mutable count : int
    }

  let create () = { buf = Byte_buf.create 4096; byte = 0; bits = 0; count = 0 }

  let[@inline] add t b =
    if b then t.byte <- t.byte lor (1 lsl t.bits);
    t.bits <- t.bits + 1;
    if t.bits = 8
    then (
      Byte_buf.add_char t.buf (Char.unsafe_of_int t.byte);
      t.byte <- 0;
      t.bits <- 0);
    t.count <- t.count + 1
  ;;

  let flush t =
    if t.bits > 0 then Byte_buf.add_char t.buf (Char.unsafe_of_int t.byte);
    t.byte <- 0;
    t.bits <- 0
  ;;

  let reset t =
    Byte_buf.clear t.buf;
    t.byte <- 0;
    t.bits <- 0;
    t.count <- 0
  ;;
end

module Chunk = struct
  type 'a t =
    | Int of ('a -> int) * Int_col.t
    | Double of ('a -> float) * Double_col.t
    | Bytes of ('a -> string) * Bytes_col.t
    | Bool of ('a -> bool) * Bool_col.t

  let create column =
    match Schema.Column.accessor column with
    | Int32 f -> Int (f, Int_col.create ~is_32:true)
    | Int64 f -> Int (f, Int_col.create ~is_32:false)
    | Double f -> Double (f, Double_col.create ())
    | Byte_array f -> Bytes (f, Bytes_col.create ())
    | Boolean f -> Bool (f, Bool_col.create ())
  ;;

  let[@inline] add t row =
    match t with
    | Int (f, c) -> Int_col.add c (f row)
    | Double (f, c) -> Double_col.add c (f row)
    | Bytes (f, c) -> Bytes_col.add c (f row)
    | Bool (f, c) -> Bool_col.add c (f row)
  ;;

  let count = function
    | Int (_, c) -> c.count
    | Double (_, c) -> c.count
    | Bytes (_, c) -> c.count
    | Bool (_, c) -> c.count
  ;;

  let plain f =
    let buf = Buffer.create 8 in
    f buf;
    Buffer.contents buf
  ;;

  (* Returns the page payload, valid until [reset], and the statistics. *)
  let finish t ~num_rows : Byte_buf.t * M.Statistics.t =
    let stats ?min ?max () : M.Statistics.t =
      { null_count = Some 0; min_value = min; max_value = max }
    in
    match t with
    | Int (_, c) ->
      let encode v =
        plain (fun b -> if c.is_32 then Encoding.int32 b v else Encoding.int64 b v)
      in
      let stats =
        if num_rows = 0
        then stats ()
        else stats ~min:(encode c.min) ~max:(encode c.max) ()
      in
      c.buf, stats
    | Double (_, c) ->
      let stats =
        if Float.is_nan c.stats.min
        then stats ()
        else (
          (* The spec asks for -0.0 as a zero minimum and +0.0 as a zero maximum. *)
          let min = if Float.equal c.stats.min 0. then -0. else c.stats.min in
          let max = if Float.equal c.stats.max 0. then 0. else c.stats.max in
          stats
            ~min:(plain (fun b -> Encoding.double b min))
            ~max:(plain (fun b -> Encoding.double b max))
            ())
      in
      c.buf, stats
    | Bytes (_, c) ->
      let stats = if c.count = 0 then stats () else stats ~min:c.min ~max:c.max () in
      c.buf, stats
    | Bool (_, c) ->
      Bool_col.flush c;
      c.buf, stats ()
  ;;

  let reset = function
    | Int (_, c) -> Int_col.reset c
    | Double (_, c) -> Double_col.reset c
    | Bytes (_, c) -> Bytes_col.reset c
    | Bool (_, c) -> Bool_col.reset c
  ;;
end

type 'a t =
  { schema : 'a Schema.t
  ; columns : 'a Schema.Column.t array
  ; chunks : 'a Chunk.t array
  ; row_group_rows : int
  ; compression : [ `None | `Zstd ]
  ; out : Out_channel.t
  ; mutable scratch : Bytes.t (* zstd output, reused across pages *)
  ; mutable pos : int
  ; mutable rows_in_group : int
  ; mutable num_rows : int
  ; mutable row_groups : M.Row_group.t list (* newest first *)
  ; mutable closed : bool
  }

let output t s =
  Out_channel.output_string t.out s;
  t.pos <- t.pos + String.length s
;;

let output_bytes t buf ~len =
  Out_channel.output t.out ~buf ~pos:0 ~len;
  t.pos <- t.pos + len
;;

let create ~schema ?(row_group_rows = 1_048_576) ?(compression = `None) path =
  if row_group_rows < 1
  then raise_s [%message "Parquet.Writer.create: row_group_rows must be positive"];
  let columns = Array.of_list (Schema.columns schema) in
  let t =
    { schema
    ; columns
    ; chunks = Array.map columns ~f:Chunk.create
    ; row_group_rows
    ; compression
    ; out = Out_channel.create ~binary:true path
    ; scratch = Bytes.create 0
    ; pos = 0
    ; rows_in_group = 0
    ; num_rows = 0
    ; row_groups = []
    ; closed = false
    }
  in
  output t magic;
  t
;;

let write_column_chunk t column chunk ~num_rows : M.Column_chunk.t =
  let buf, statistics = Chunk.finish chunk ~num_rows in
  let uncompressed_size = Byte_buf.length buf in
  let (codec : M.Compression_codec.t), data, compressed_size =
    match t.compression with
    | `None -> Uncompressed, Byte_buf.unsafe_bytes buf, uncompressed_size
    | `Zstd ->
      let bound = Zstd_bytes.compress_bound uncompressed_size in
      if Bytes.length t.scratch < bound then t.scratch <- Bytes.create bound;
      let n =
        Zstd_bytes.compress
          ~level:zstd_level
          (Byte_buf.unsafe_bytes buf)
          ~len:uncompressed_size
          ~dst:t.scratch
      in
      Zstd, t.scratch, n
  in
  let header : M.Page_header.t =
    { type_ = Data_page
    ; uncompressed_page_size = uncompressed_size
    ; compressed_page_size = compressed_size
    ; data_page_header =
        Some
          { num_values = num_rows
          ; encoding = Plain
          ; definition_level_encoding = Rle
          ; repetition_level_encoding = Rle
          ; statistics = Some statistics
          }
    ; dictionary_page_header = None
    }
  in
  let header = Thrift_compact.to_string (M.Page_header.to_thrift header) in
  let offset = t.pos in
  output t header;
  output_bytes t data ~len:compressed_size;
  Chunk.reset chunk;
  let header_size = String.length header in
  { file_offset = offset
  ; meta_data =
      { type_ = Schema.Column.physical_type column
      ; encodings = [ Plain ]
      ; path_in_schema = [ Schema.Column.name column ]
      ; codec
      ; num_values = num_rows
      ; total_uncompressed_size = header_size + uncompressed_size
      ; total_compressed_size = header_size + compressed_size
      ; data_page_offset = offset
      ; dictionary_page_offset = None
      ; statistics = Some statistics
      }
  }
;;

let flush_row_group t =
  if t.rows_in_group > 0
  then (
    let num_rows = t.rows_in_group in
    let start = t.pos in
    let columns =
      Array.to_list
        (Array.mapi t.columns ~f:(fun i column ->
           write_column_chunk t column t.chunks.(i) ~num_rows))
    in
    let total_uncompressed =
      List.sum (module Int) columns ~f:(fun c -> c.meta_data.total_uncompressed_size)
    in
    let row_group : M.Row_group.t =
      { columns
      ; total_byte_size = total_uncompressed
      ; num_rows
      ; file_offset = Some start
      ; total_compressed_size = Some (t.pos - start)
      ; ordinal = Some (List.length t.row_groups)
      }
    in
    t.row_groups <- row_group :: t.row_groups;
    t.rows_in_group <- 0)
;;

let check_open t name =
  if t.closed then raise_s [%message "Parquet.Writer: writer is closed" (name : string)]
;;

let finish_row t =
  t.rows_in_group <- t.rows_in_group + 1;
  t.num_rows <- t.num_rows + 1;
  if t.rows_in_group >= t.row_group_rows then flush_row_group t
;;

let add t row =
  check_open t "add";
  let chunks = t.chunks in
  for i = 0 to Array.length chunks - 1 do
    Chunk.add (Array.unsafe_get chunks i) row
  done;
  finish_row t
;;

module Direct = struct
  type int_column = Int_col.t
  type float_column = Double_col.t
  type string_column = Bytes_col.t
  type bool_column = Bool_col.t

  let find t name ~kind ~f =
    match
      Array.find_mapi t.columns ~f:(fun i c ->
        if String.equal (Schema.Column.name c) name then Some t.chunks.(i) else None)
    with
    | None -> raise_s [%message "Parquet.Writer.Direct: no such column" (name : string)]
    | Some chunk ->
      (match f chunk with
       | Some c -> c
       | None ->
         raise_s
           [%message
             "Parquet.Writer.Direct: column has another type"
               (name : string)
               (kind : string)])
  ;;

  let int t name =
    find t name ~kind:"int" ~f:(function
      | Chunk.Int (_, c) -> Some c
      | _ -> None)
  ;;

  let float t name =
    find t name ~kind:"float" ~f:(function
      | Chunk.Double (_, c) -> Some c
      | _ -> None)
  ;;

  let string t name =
    find t name ~kind:"string" ~f:(function
      | Chunk.Bytes (_, c) -> Some c
      | _ -> None)
  ;;

  let bool t name =
    find t name ~kind:"bool" ~f:(function
      | Chunk.Bool (_, c) -> Some c
      | _ -> None)
  ;;

  let add_int = Int_col.add
  let add_float = Double_col.add
  let add_float_u = Double_col.add_u
  let add_string = Bytes_col.add
  let add_bool = Bool_col.add

  let[@cold] bad_row t i =
    let name = Schema.Column.name t.columns.(i) in
    let values = Chunk.count t.chunks.(i) - t.rows_in_group in
    raise_s
      [%message
        "Parquet.Writer.Direct.end_row: a column needs exactly one value per row"
          (name : string)
          (values : int)]
  ;;

  let end_row t =
    check_open t "Direct.end_row";
    let chunks = t.chunks in
    let expected = t.rows_in_group + 1 in
    for i = 0 to Array.length chunks - 1 do
      if Chunk.count (Array.unsafe_get chunks i) <> expected then bad_row t i
    done;
    finish_row t
  ;;
end

let add_all t rows = List.iter rows ~f:(add t)
let num_rows t = t.num_rows

let close t =
  if not t.closed
  then (
    flush_row_group t;
    let footer : M.File_meta_data.t =
      { version = 1
      ; schema = Schema.schema_elements t.schema
      ; num_rows = t.num_rows
      ; row_groups = List.rev t.row_groups
      ; key_value_metadata = []
      ; created_by = Some created_by
      ; type_defined_order = true
      }
    in
    let footer = Thrift_compact.to_string (M.File_meta_data.to_thrift footer) in
    output t footer;
    let len = Buffer.create 4 in
    Encoding.int32 len (String.length footer);
    output t (Buffer.contents len);
    output t magic;
    Out_channel.close t.out;
    t.closed <- true)
;;

let with_file ~schema ?row_group_rows ?compression path ~f =
  let t = create ~schema ?row_group_rows ?compression path in
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> close t)
;;
