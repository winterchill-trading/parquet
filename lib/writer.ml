open! Core
module M = Metadata

let magic = "PAR1"
let created_by = "parquet-ocaml version 0.1.0"
let zstd_level = 3

(* Per-column buffer for the current row group, with running statistics. *)
module Chunk = struct
  type 'a t =
    | Int of
        { f : 'a -> int
        ; is_32 : bool
        ; buf : Buffer.t
        ; mutable min : int
        ; mutable max : int
        }
    | Double of
        { f : 'a -> float
        ; buf : Buffer.t
        ; mutable min : float
        ; mutable max : float
        }
    | Bytes of
        { f : 'a -> string
        ; buf : Buffer.t
        ; mutable min : string option
        ; mutable max : string option
        }
    | Bool of
        { f : 'a -> bool
        ; buf : Buffer.t
        ; bits : Encoding.Booleans.t
        }

  let create (accessor : 'a Schema.Column.Accessor.t) =
    let buf = Buffer.create 4096 in
    match accessor with
    | Int32 f -> Int { f; is_32 = true; buf; min = Int.max_value; max = Int.min_value }
    | Int64 f -> Int { f; is_32 = false; buf; min = Int.max_value; max = Int.min_value }
    | Double f -> Double { f; buf; min = Float.nan; max = Float.nan }
    | Byte_array f -> Bytes { f; buf; min = None; max = None }
    | Boolean f -> Bool { f; buf; bits = Encoding.Booleans.create () }
  ;;

  let add t row =
    match t with
    | Int c ->
      let v = c.f row in
      if c.is_32 then Encoding.int32 c.buf v else Encoding.int64 c.buf v;
      if v < c.min then c.min <- v;
      if v > c.max then c.max <- v
    | Double c ->
      let v = c.f row in
      Encoding.double c.buf v;
      (* NaN is not ordered and must not appear in statistics. *)
      if not (Float.is_nan v)
      then (
        if Float.is_nan c.min || Float.( < ) v c.min then c.min <- v;
        if Float.is_nan c.max || Float.( > ) v c.max then c.max <- v)
    | Bytes c ->
      let v = c.f row in
      Encoding.byte_array c.buf v;
      (match c.min with
       | Some m when String.( >= ) v m -> ()
       | _ -> c.min <- Some v);
      (match c.max with
       | Some m when String.( <= ) v m -> ()
       | _ -> c.max <- Some v)
    | Bool c -> Encoding.Booleans.add c.bits c.buf (c.f row)
  ;;

  let plain f =
    let buf = Buffer.create 8 in
    f buf;
    Buffer.contents buf
  ;;

  (* Returns the page payload and statistics, and resets the chunk. *)
  let finish t ~num_rows : string * M.Statistics.t =
    let stats ?min ?max () : M.Statistics.t =
      { null_count = Some 0; min_value = min; max_value = max }
    in
    match t with
    | Int c ->
      let data = Buffer.contents c.buf in
      let encode v =
        plain (fun b -> if c.is_32 then Encoding.int32 b v else Encoding.int64 b v)
      in
      let stats =
        if num_rows = 0
        then stats ()
        else stats ~min:(encode c.min) ~max:(encode c.max) ()
      in
      Buffer.clear c.buf;
      c.min <- Int.max_value;
      c.max <- Int.min_value;
      data, stats
    | Double c ->
      let data = Buffer.contents c.buf in
      let stats =
        if Float.is_nan c.min
        then stats ()
        else (
          (* The spec asks for -0.0 as a zero minimum and +0.0 as a zero maximum. *)
          let min = if Float.equal c.min 0. then -0. else c.min in
          let max = if Float.equal c.max 0. then 0. else c.max in
          stats
            ~min:(plain (fun b -> Encoding.double b min))
            ~max:(plain (fun b -> Encoding.double b max))
            ())
      in
      Buffer.clear c.buf;
      c.min <- Float.nan;
      c.max <- Float.nan;
      data, stats
    | Bytes c ->
      let data = Buffer.contents c.buf in
      let stats = stats ?min:c.min ?max:c.max () in
      Buffer.clear c.buf;
      c.min <- None;
      c.max <- None;
      data, stats
    | Bool c ->
      Encoding.Booleans.flush c.bits c.buf;
      let data = Buffer.contents c.buf in
      Buffer.clear c.buf;
      data, stats ()
  ;;
end

type 'a t =
  { schema : 'a Schema.t
  ; columns : 'a Schema.Column.t array
  ; chunks : 'a Chunk.t array
  ; row_group_rows : int
  ; compression : [ `None | `Zstd ]
  ; out : Out_channel.t
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

let create ~schema ?(row_group_rows = 1_048_576) ?(compression = `None) path =
  if row_group_rows < 1
  then raise_s [%message "Parquet.Writer.create: row_group_rows must be positive"];
  let columns = Array.of_list (Schema.columns schema) in
  let t =
    { schema
    ; columns
    ; chunks = Array.map columns ~f:(fun c -> Chunk.create (Schema.Column.accessor c))
    ; row_group_rows
    ; compression
    ; out = Out_channel.create ~binary:true path
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
  let data, statistics = Chunk.finish chunk ~num_rows in
  let uncompressed_size = String.length data in
  let (codec : M.Compression_codec.t), data =
    match t.compression with
    | `None -> Uncompressed, data
    | `Zstd -> Zstd, Zstd.compress ~level:zstd_level data
  in
  let compressed_size = String.length data in
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
    }
  in
  let header = Thrift_compact.to_string (M.Page_header.to_thrift header) in
  let offset = t.pos in
  output t header;
  output t data;
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

let add t row =
  check_open t "add";
  Array.iter t.chunks ~f:(fun chunk -> Chunk.add chunk row);
  t.rows_in_group <- t.rows_in_group + 1;
  t.num_rows <- t.num_rows + 1;
  if t.rows_in_group >= t.row_group_rows then flush_row_group t
;;

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
