open! Core
module M = Metadata

let magic = "PAR1"
let created_by = "parquet-ocaml version 0.1.0"
let zstd_level = 3

(* Typed column builders for the current row group, with running statistics. Values are
   appended by [Writer.add] through the schema accessors or directly by the caller through
   [Direct]; [count] (values and nulls) lets [Direct.end_row] check that every column got
   one value. *)

(* Definition levels of an optional column: one byte per row, 1 when the value is present,
   0 when it is null; RLE-encoded when the page is written. *)
module Levels = struct
  type t =
    { buf : Byte_buf.t
    ; mutable nulls : int
    }

  let create ~optional =
    if optional then Some { buf = Byte_buf.create 4096; nulls = 0 } else None
  ;;

  let[@inline] present = function
    | None -> ()
    | Some t -> Byte_buf.add_char t.buf '\001'
  ;;

  let null t =
    Byte_buf.add_char t.buf '\000';
    t.nulls <- t.nulls + 1
  ;;

  let nulls = function
    | None -> 0
    | Some t -> t.nulls
  ;;

  let reset = function
    | None -> ()
    | Some t ->
      Byte_buf.clear t.buf;
      t.nulls <- 0
  ;;
end

(* Dictionary of a column chunk: the distinct values in first-seen order, PLAIN-encoded in
   [entries], and one int32 index per present value in [indices]. Past [max_bytes] of
   entries the chunk stops using it and is written PLAIN. *)
module Dict = struct
  type 'k t =
    { index : ('k, int) Hashtbl.t
    ; encode : Byte_buf.t -> 'k -> unit
    ; entries : Byte_buf.t
    ; indices : Byte_buf.t
    ; mutable overflowed : bool
    }

  let max_bytes = 1 lsl 20

  let create ~dictionary key ~encode =
    if dictionary
    then
      Some
        { index = Hashtbl.create key
        ; encode
        ; entries = Byte_buf.create 4096
        ; indices = Byte_buf.create 4096
        ; overflowed = false
        }
    else None
  ;;

  let add t key =
    if not t.overflowed
    then (
      let i =
        match Hashtbl.find t.index key with
        | Some i -> i
        | None ->
          let i = Hashtbl.length t.index in
          Hashtbl.set t.index ~key ~data:i;
          t.encode t.entries key;
          i
      in
      Byte_buf.add_int32_le t.indices i;
      if Byte_buf.length t.entries > max_bytes
      then (
        t.overflowed <- true;
        Hashtbl.clear t.index))
  ;;

  let reset t =
    Hashtbl.clear t.index;
    Byte_buf.clear t.entries;
    Byte_buf.clear t.indices;
    t.overflowed <- false
  ;;

  let size t = Hashtbl.length t.index
end

(* What a column chunk hands to the page writer. *)
module Finished = struct
  type dictionary =
    { entries : Byte_buf.t
    ; num_entries : int
    ; indices : Byte_buf.t
    }

  type t =
    { values : Byte_buf.t (* PLAIN, present values only *)
    ; dictionary : dictionary option
    ; levels : Levels.t option
    ; statistics : M.Statistics.t
    }

  let dictionary = function
    (* A chunk of nulls only has no values to index; it is written PLAIN. *)
    | Some (d : _ Dict.t) when (not d.overflowed) && Dict.size d > 0 ->
      Some { entries = d.entries; num_entries = Dict.size d; indices = d.indices }
    | _ -> None
  ;;
end

module Int_col = struct
  type t =
    { is_32 : bool
    ; buf : Byte_buf.t
    ; levels : Levels.t option
    ; dict : int Dict.t option
    ; mutable min : int
    ; mutable max : int
    ; mutable count : int
    }

  let create ~is_32 ~optional ~dictionary =
    { is_32
    ; buf = Byte_buf.create 4096
    ; levels = Levels.create ~optional
    ; dict =
        Dict.create
          ~dictionary
          (module Int)
          ~encode:(if is_32 then Byte_buf.add_int32_le else Byte_buf.add_int64_le)
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
    Levels.present t.levels;
    (match t.dict with
     | None -> ()
     | Some d -> Dict.add d v);
    t.count <- t.count + 1
  ;;

  let reset t =
    Byte_buf.clear t.buf;
    Levels.reset t.levels;
    Option.iter t.dict ~f:Dict.reset;
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
    ; levels : Levels.t option
    ; dict : Int64.t Dict.t option (* keyed by the bits, so NaNs and -0. are kept *)
    ; stats : stats
    ; mutable count : int
    }

  let create ~optional ~dictionary =
    { buf = Byte_buf.create 4096
    ; levels = Levels.create ~optional
    ; dict =
        Dict.create ~dictionary (module Int64) ~encode:(fun buf bits ->
          Byte_buf.add_double_le buf (Int64.float_of_bits bits))
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
    Levels.present t.levels;
    (match t.dict with
     | None -> ()
     | Some d -> Dict.add d (Int64.bits_of_float v));
    t.count <- t.count + 1
  ;;

  let[@inline] add t v = add_u t (unbox_float v)

  let reset t =
    Byte_buf.clear t.buf;
    Levels.reset t.levels;
    Option.iter t.dict ~f:Dict.reset;
    t.stats.min <- Float.nan;
    t.stats.max <- Float.nan;
    t.count <- 0
  ;;
end

module Bytes_col = struct
  (* [min] and [max] are meaningful only when a value is present. *)
  type t =
    { buf : Byte_buf.t
    ; levels : Levels.t option
    ; dict : string Dict.t option
    ; mutable min : string
    ; mutable max : string
    ; mutable count : int
    }

  let encode buf v =
    Byte_buf.add_int32_le buf (String.length v);
    Byte_buf.add_string buf v
  ;;

  let create ~optional ~dictionary =
    { buf = Byte_buf.create 4096
    ; levels = Levels.create ~optional
    ; dict = Dict.create ~dictionary (module String) ~encode
    ; min = ""
    ; max = ""
    ; count = 0
    }
  ;;

  let[@inline] add t v =
    Byte_buf.add_int32_le t.buf (String.length v);
    Byte_buf.add_string t.buf v;
    if t.count = Levels.nulls t.levels
    then (
      t.min <- v;
      t.max <- v)
    else (
      if String.( < ) v t.min then t.min <- v;
      if String.( > ) v t.max then t.max <- v);
    Levels.present t.levels;
    (match t.dict with
     | None -> ()
     | Some d -> Dict.add d v);
    t.count <- t.count + 1
  ;;

  let reset t =
    Byte_buf.clear t.buf;
    Levels.reset t.levels;
    Option.iter t.dict ~f:Dict.reset;
    t.min <- "";
    t.max <- "";
    t.count <- 0
  ;;
end

module Bool_col = struct
  (* Bit-packed, least significant bit first; [byte] holds the [bits] pending bits. *)
  type t =
    { buf : Byte_buf.t
    ; levels : Levels.t option
    ; mutable byte : int
    ; mutable bits : int
    ; mutable count : int
    }

  let create ~optional =
    { buf = Byte_buf.create 4096
    ; levels = Levels.create ~optional
    ; byte = 0
    ; bits = 0
    ; count = 0
    }
  ;;

  let[@inline] add t b =
    if b then t.byte <- t.byte lor (1 lsl t.bits);
    t.bits <- t.bits + 1;
    if t.bits = 8
    then (
      Byte_buf.add_char t.buf (Char.unsafe_of_int t.byte);
      t.byte <- 0;
      t.bits <- 0);
    Levels.present t.levels;
    t.count <- t.count + 1
  ;;

  let flush t =
    if t.bits > 0 then Byte_buf.add_char t.buf (Char.unsafe_of_int t.byte);
    t.byte <- 0;
    t.bits <- 0
  ;;

  let reset t =
    Byte_buf.clear t.buf;
    Levels.reset t.levels;
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
    | Optional : ('a -> 'b option) * 'b t -> 'a t

  let of_accessor
    : type a. a Schema.Column.Accessor.t -> optional:bool -> dictionary:bool -> a t
    =
    fun accessor ~optional ~dictionary ->
    match accessor with
    | Int32 f -> Int (f, Int_col.create ~is_32:true ~optional ~dictionary)
    | Int64 f -> Int (f, Int_col.create ~is_32:false ~optional ~dictionary)
    | Double f -> Double (f, Double_col.create ~optional ~dictionary)
    | Byte_array f -> Bytes (f, Bytes_col.create ~optional ~dictionary)
    | Boolean f -> Bool (f, Bool_col.create ~optional)
  ;;

  let create column =
    let dictionary = Schema.Column.is_dictionary column in
    match Schema.Column.kind column with
    | Required accessor -> of_accessor accessor ~optional:false ~dictionary
    | Optional (f, accessor) ->
      Optional (f, of_accessor accessor ~optional:true ~dictionary)
  ;;

  let rec levels : type a. a t -> Levels.t option = function
    | Int (_, c) -> c.levels
    | Double (_, c) -> c.levels
    | Bytes (_, c) -> c.levels
    | Bool (_, c) -> c.levels
    | Optional (_, inner) -> levels inner
  ;;

  let[@cold] null_in_required () =
    raise_s [%message "Parquet.Writer: null in a column that is not optional"]
  ;;

  let levels_exn = function
    | None -> null_in_required ()
    | Some levels -> levels
  ;;

  let rec add_null : type a. a t -> unit = function
    | Int (_, c) ->
      Levels.null (levels_exn c.levels);
      c.count <- c.count + 1
    | Double (_, c) ->
      Levels.null (levels_exn c.levels);
      c.count <- c.count + 1
    | Bytes (_, c) ->
      Levels.null (levels_exn c.levels);
      c.count <- c.count + 1
    | Bool (_, c) ->
      Levels.null (levels_exn c.levels);
      c.count <- c.count + 1
    | Optional (_, inner) -> add_null inner
  ;;

  let rec add : type a. a t -> a -> unit =
    fun t row ->
    match t with
    | Int (f, c) -> Int_col.add c (f row)
    | Double (f, c) -> Double_col.add c (f row)
    | Bytes (f, c) -> Bytes_col.add c (f row)
    | Bool (f, c) -> Bool_col.add c (f row)
    | Optional (f, inner) ->
      (match f row with
       | Some v -> add inner v
       | None -> add_null inner)
  ;;

  let rec count : type a. a t -> int = function
    | Int (_, c) -> c.count
    | Double (_, c) -> c.count
    | Bytes (_, c) -> c.count
    | Bool (_, c) -> c.count
    | Optional (_, inner) -> count inner
  ;;

  let plain f =
    let buf = Buffer.create 8 in
    f buf;
    Buffer.contents buf
  ;;

  (* The page payloads, valid until [reset], and the statistics. *)
  let rec finish : type a. a t -> Finished.t = function
    | Optional (_, inner) -> finish inner
    | t ->
      let levels = levels t in
      let nulls = Levels.nulls levels in
      let stats ?min ?max () : M.Statistics.t =
        { null_count = Some nulls; min_value = min; max_value = max }
      in
      let finished ?dict values statistics : Finished.t =
        { values; dictionary = Finished.dictionary dict; levels; statistics }
      in
      (match t with
       | Optional _ -> assert false
       | Int (_, c) ->
         let encode v =
           plain (fun b -> if c.is_32 then Encoding.int32 b v else Encoding.int64 b v)
         in
         let stats =
           if c.count = nulls
           then stats ()
           else stats ~min:(encode c.min) ~max:(encode c.max) ()
         in
         finished ?dict:c.dict c.buf stats
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
         finished ?dict:c.dict c.buf stats
       | Bytes (_, c) ->
         let stats =
           if c.count = nulls then stats () else stats ~min:c.min ~max:c.max ()
         in
         finished ?dict:c.dict c.buf stats
       | Bool (_, c) ->
         Bool_col.flush c;
         finished c.buf (stats ()))
  ;;

  let rec reset : type a. a t -> unit = function
    | Int (_, c) -> Int_col.reset c
    | Double (_, c) -> Double_col.reset c
    | Bytes (_, c) -> Bytes_col.reset c
    | Bool (_, c) -> Bool_col.reset c
    | Optional (_, inner) -> reset inner
  ;;
end

type 'a t =
  { schema : 'a Schema.t
  ; columns : 'a Schema.Column.t array
  ; chunks : 'a Chunk.t array
  ; row_group_rows : int
  ; compression : [ `None | `Zstd ]
  ; key_value_metadata : M.Key_value.t list
  ; out : Out_channel.t
  ; mutable scratch : Bytes.t (* zstd output, reused across pages *)
  ; page : Byte_buf.t (* page body with levels or dictionary indices *)
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

let create
  ~schema
  ?(row_group_rows = 1_048_576)
  ?(compression = `None)
  ?(key_value_metadata = [])
  path
  =
  if row_group_rows < 1
  then raise_s [%message "Parquet.Writer.create: row_group_rows must be positive"];
  let columns = Array.of_list (Schema.columns schema) in
  let t =
    { schema
    ; columns
    ; chunks = Array.map columns ~f:Chunk.create
    ; row_group_rows
    ; compression
    ; key_value_metadata =
        List.map key_value_metadata ~f:(fun (key, value) : M.Key_value.t ->
          { key; value = Some value })
    ; out = Out_channel.create ~binary:true path
    ; scratch = Bytes.create 0
    ; page = Byte_buf.create 0
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

(* Writes one page (header, then the body compressed or not) and returns the codec, the
   header and body sizes, uncompressed and as written. *)
let write_page t ~(page : M.Page_header.t) body =
  let uncompressed_size = Byte_buf.length body in
  let (codec : M.Compression_codec.t), data, compressed_size =
    match t.compression with
    | `None -> Uncompressed, Byte_buf.unsafe_bytes body, uncompressed_size
    | `Zstd ->
      let bound = Zstd_bytes.compress_bound uncompressed_size in
      if Bytes.length t.scratch < bound then t.scratch <- Bytes.create bound;
      let n =
        Zstd_bytes.compress
          ~level:zstd_level
          (Byte_buf.unsafe_bytes body)
          ~len:uncompressed_size
          ~dst:t.scratch
      in
      Zstd, t.scratch, n
  in
  let header =
    Thrift_compact.to_string
      (M.Page_header.to_thrift
         { page with
           uncompressed_page_size = uncompressed_size
         ; compressed_page_size = compressed_size
         })
  in
  output t header;
  output_bytes t data ~len:compressed_size;
  let header_size = String.length header in
  codec, header_size + uncompressed_size, header_size + compressed_size
;;

let add_buf dst src =
  Byte_buf.add_bytes dst (Byte_buf.unsafe_bytes src) ~pos:0 ~len:(Byte_buf.length src)
;;

(* The data page body when it is more than the PLAIN values: definition levels (length
   prefixed in a v1 page), then either the values or the dictionary indices (bit width
   byte, then RLE/bit-packed hybrid). *)
let data_page_body t (finished : Finished.t) ~num_rows =
  match finished.levels, finished.dictionary with
  | None, None -> finished.values
  | levels, dictionary ->
    let page = t.page in
    Byte_buf.clear page;
    Option.iter levels ~f:(fun levels ->
      let start = Byte_buf.length page in
      Byte_buf.add_int32_le page 0;
      let bytes = Byte_buf.unsafe_bytes levels.buf in
      Rle_hybrid.encode
        ~bit_width:1
        ~len:num_rows
        ~get:(fun i -> Char.to_int (Bytes.unsafe_get bytes i))
        page;
      Byte_buf.set_int32_le page ~pos:start (Byte_buf.length page - start - 4));
    (match dictionary with
     | None -> add_buf page finished.values
     | Some { entries = _; num_entries; indices } ->
       let bit_width = Rle_hybrid.bit_width ~max_value:(num_entries - 1) in
       Byte_buf.add_char page (Char.of_int_exn bit_width);
       let bytes = Byte_buf.unsafe_bytes indices in
       Rle_hybrid.encode
         ~bit_width
         ~len:(Byte_buf.length indices / 4)
         ~get:(fun i -> Stdlib.Bytes.get_int32_le bytes (4 * i) |> Int32.to_int_trunc)
         page);
    page
;;

let write_column_chunk t column chunk ~num_rows : M.Column_chunk.t =
  let finished = Chunk.finish chunk in
  let start = t.pos in
  let dictionary_page =
    Option.map finished.dictionary ~f:(fun d ->
      let page : M.Page_header.t =
        { type_ = Dictionary_page
        ; uncompressed_page_size = 0
        ; compressed_page_size = 0
        ; data_page_header = None
        ; dictionary_page_header =
            Some
              { num_values = d.num_entries
              ; encoding = Plain_dictionary
              ; is_sorted = None
              }
        }
      in
      write_page t ~page d.entries)
  in
  let encoding : M.Encoding.t =
    if Option.is_some finished.dictionary then Plain_dictionary else Plain
  in
  let page : M.Page_header.t =
    { type_ = Data_page
    ; uncompressed_page_size = 0
    ; compressed_page_size = 0
    ; data_page_header =
        Some
          { num_values = num_rows
          ; encoding
          ; definition_level_encoding = Rle
          ; repetition_level_encoding = Rle
          ; statistics = Some finished.statistics
          }
    ; dictionary_page_header = None
    }
  in
  let data_page_offset = t.pos in
  let codec, uncompressed, compressed =
    write_page t ~page (data_page_body t finished ~num_rows)
  in
  Chunk.reset chunk;
  let dict_uncompressed, dict_compressed =
    match dictionary_page with
    | None -> 0, 0
    | Some (_, u, c) -> u, c
  in
  { file_offset = start
  ; meta_data =
      { type_ = Schema.Column.physical_type column
      ; encodings =
          encoding :: (if Option.is_some finished.levels then [ M.Encoding.Rle ] else [])
      ; path_in_schema = [ Schema.Column.name column ]
      ; codec
      ; num_values = num_rows
      ; total_uncompressed_size = dict_uncompressed + uncompressed
      ; total_compressed_size = dict_compressed + compressed
      ; data_page_offset
      ; dictionary_page_offset = Option.map dictionary_page ~f:(fun _ -> start)
      ; statistics = Some finished.statistics
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

  (* An optional column's typed builder sits under [Chunk.Optional]. *)
  type packed = Packed : _ Chunk.t -> packed

  let rec unwrap : type a. a Chunk.t -> packed = function
    | Optional (_, inner) -> unwrap inner
    | chunk -> Packed chunk
  ;;

  let find t name ~kind ~f =
    match
      Array.find_mapi t.columns ~f:(fun i c ->
        if String.equal (Schema.Column.name c) name then Some t.chunks.(i) else None)
    with
    | None -> raise_s [%message "Parquet.Writer.Direct: no such column" (name : string)]
    | Some chunk ->
      (match f (unwrap chunk) with
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
      | Packed (Chunk.Int (_, c)) -> Some c
      | Packed _ -> None)
  ;;

  let float t name =
    find t name ~kind:"float" ~f:(function
      | Packed (Chunk.Double (_, c)) -> Some c
      | Packed _ -> None)
  ;;

  let string t name =
    find t name ~kind:"string" ~f:(function
      | Packed (Chunk.Bytes (_, c)) -> Some c
      | Packed _ -> None)
  ;;

  let bool t name =
    find t name ~kind:"bool" ~f:(function
      | Packed (Chunk.Bool (_, c)) -> Some c
      | Packed _ -> None)
  ;;

  let add_int = Int_col.add
  let add_float = Double_col.add
  let add_float_u = Double_col.add_u
  let add_string = Bytes_col.add
  let add_bool = Bool_col.add

  let add_null_int (c : int_column) =
    Levels.null (Chunk.levels_exn c.levels);
    c.count <- c.count + 1
  ;;

  let add_null_float (c : float_column) =
    Levels.null (Chunk.levels_exn c.levels);
    c.count <- c.count + 1
  ;;

  let add_null_string (c : string_column) =
    Levels.null (Chunk.levels_exn c.levels);
    c.count <- c.count + 1
  ;;

  let add_null_bool (c : bool_column) =
    Levels.null (Chunk.levels_exn c.levels);
    c.count <- c.count + 1
  ;;

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
      ; key_value_metadata = t.key_value_metadata
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

let with_file ~schema ?row_group_rows ?compression ?key_value_metadata path ~f =
  let t = create ~schema ?row_group_rows ?compression ?key_value_metadata path in
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> close t)
;;
