open! Core
module M = Metadata

module Value = struct
  type t =
    | Null
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
  [@@deriving sexp_of, compare, equal]
end

type t =
  { path : string
  ; ic : In_channel.t
  ; metadata : M.File_meta_data.t
  ; columns : M.Schema_element.t array
  }

let magic = "PAR1"

let[@cold] fail what (details : Sexp.t) =
  raise_s (List [ Atom ("Parquet.Reader: " ^ what); details ])
;;

let[@cold] fail0 what = raise_s (Atom ("Parquet.Reader: " ^ what))

let read_at ic ~pos ~len =
  In_channel.seek ic (Int64.of_int pos);
  let buf = Bytes.create len in
  In_channel.really_input_exn ic ~buf ~pos:0 ~len;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
;;

let int32_le s pos =
  if pos < 0 || pos + 4 > String.length s then fail0 "truncated length";
  Stdlib.String.get_int32_le s pos |> Int32.to_int_trunc
;;

(* The schema must be a root followed by its leaf columns. *)
let flat_columns (metadata : M.File_meta_data.t) =
  match metadata.schema with
  | [] -> fail0 "empty schema"
  | root :: leaves ->
    let n = List.length leaves in
    if not ([%equal: int option] root.num_children (Some n))
    then
      fail
        "nested schemas are not supported"
        [%message (root.num_children : int option) (n : int)];
    List.iter leaves ~f:(fun (e : M.Schema_element.t) ->
      (match e.num_children with
       | None | Some 0 -> ()
       | Some _ -> fail "nested schemas are not supported" [%message e.name]);
      (match e.repetition_type with
       | Some Repeated -> fail "repeated columns are not supported" [%message e.name]
       | Some (Required | Optional) | None -> ());
      if Option.is_none e.type_ then fail "column without a type" [%message e.name]);
    Array.of_list leaves
;;

let create path =
  Or_error.try_with_join (fun () ->
    let ic = In_channel.create ~binary:true path in
    match
      Or_error.try_with (fun () ->
        let size = Int64.to_int_exn (In_channel.length ic) in
        if size < 12 then fail0 "file too short";
        if not (String.equal (read_at ic ~pos:0 ~len:4) magic)
        then fail0 "missing leading magic";
        let tail = read_at ic ~pos:(size - 8) ~len:8 in
        if not (String.equal (String.sub tail ~pos:4 ~len:4) magic)
        then fail0 "missing trailing magic (encrypted or truncated file?)";
        let footer_len = int32_le tail 0 in
        if footer_len < 0 || footer_len > size - 12 then fail0 "bad footer length";
        let footer = read_at ic ~pos:(size - 8 - footer_len) ~len:footer_len in
        let metadata = M.File_meta_data.of_thrift (Thrift_compact.of_string footer) in
        { path; ic; metadata; columns = flat_columns metadata })
    with
    | Ok t -> Ok t
    | Error e ->
      In_channel.close ic;
      Error e)
  |> Or_error.tag ~tag:path
;;

let close t = In_channel.close t.ic

let with_file path ~f =
  let%bind.Or_error t = create path in
  Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> close t)
;;

let metadata t = t.metadata
let columns t = Array.to_list t.columns
let num_rows t = t.metadata.num_rows
let num_row_groups t = List.length t.metadata.row_groups

(* PLAIN decoding of [n] values of a physical type starting at [pos]; returns the values
   and the position after them. *)
let decode_plain (column : M.Schema_element.t) s ~pos ~n : Value.t array * int =
  let len = String.length s in
  let need p k = if p + k > len then fail "truncated values" [%message column.name] in
  let p = ref pos in
  let fixed k f =
    need !p (n * k);
    let out =
      Array.init n ~f:(fun i ->
        let v = f (!p + (i * k)) in
        v)
    in
    p := !p + (n * k);
    out
  in
  let values : Value.t array =
    match column.type_ with
    | None -> fail0 "column without a type"
    | Some Boolean ->
      need !p ((n + 7) / 8);
      let out =
        Array.init n ~f:(fun i ->
          let byte = Char.to_int s.[!p + (i / 8)] in
          Value.Bool (byte land (1 lsl (i % 8)) <> 0))
      in
      p := !p + ((n + 7) / 8);
      out
    | Some Int32 -> fixed 4 (fun q -> Value.Int (int32_le s q))
    | Some Int64 ->
      fixed 8 (fun q ->
        let v = Stdlib.String.get_int64_le s q in
        match Int64.to_int v with
        | Some v -> Value.Int v
        | None ->
          fail "INT64 value outside the int range" [%message column.name (v : int64)])
    | Some Float ->
      fixed 4 (fun q ->
        Value.Float (Int32.float_of_bits (Stdlib.String.get_int32_le s q)))
    | Some Double ->
      fixed 8 (fun q ->
        Value.Float (Int64.float_of_bits (Stdlib.String.get_int64_le s q)))
    | Some Fixed_len_byte_array ->
      (match column.type_length with
       | Some k when k >= 0 ->
         fixed k (fun q -> Value.String (String.sub s ~pos:q ~len:k))
       | _ -> fail "FIXED_LEN_BYTE_ARRAY without a length" [%message column.name])
    | Some Byte_array ->
      Array.init n ~f:(fun _ ->
        let k = int32_le s !p in
        if k < 0 then fail "negative byte array length" [%message column.name];
        need (!p + 4) k;
        let v = String.sub s ~pos:(!p + 4) ~len:k in
        p := !p + 4 + k;
        Value.String v)
    | Some Int96 -> fail "INT96 is not supported" [%message column.name]
  in
  values, !p
;;

let decompress (codec : M.Compression_codec.t) body ~uncompressed_size =
  match codec with
  | Uncompressed -> body
  | Zstd ->
    let out = Zstd.decompress uncompressed_size body in
    if String.length out <> uncompressed_size then fail0 "zstd page size mismatch";
    out
  | Snappy ->
    let out = Decompress.snappy body in
    if String.length out <> uncompressed_size then fail0 "snappy page size mismatch";
    out
  | Lz4_raw -> Decompress.lz4_raw body ~uncompressed_size
  | (Gzip | Lzo | Brotli | Lz4) as codec ->
    fail "unsupported compression codec" [%message (codec : M.Compression_codec.t)]
;;

(* Values of one v1 data page: [n] slots, nulls where the definition level is 0. *)
let decode_data_page
  (column : M.Schema_element.t)
  ~dictionary
  (header : M.Data_page_header.t)
  page
  : Value.t array
  =
  let n = header.num_values in
  let optional =
    match column.repetition_type with
    | Some Optional -> true
    | Some Required | Some Repeated | None -> false
  in
  let levels, pos =
    if optional
    then (
      let len = int32_le page 0 in
      let levels, _ = Rle_hybrid.decode ~bit_width:1 page ~pos:4 ~stop:(4 + len) ~len:n in
      Some levels, 4 + len)
    else None, 0
  in
  let present =
    match levels with
    | None -> n
    | Some levels -> Array.count levels ~f:(fun l -> l = 1)
  in
  let values =
    match header.encoding with
    | Plain -> fst (decode_plain column page ~pos ~n:present)
    | Plain_dictionary | Rle_dictionary ->
      let dictionary =
        match dictionary with
        | Some d -> d
        | None ->
          fail "dictionary-encoded page without a dictionary" [%message column.name]
      in
      if pos >= String.length page && present > 0
      then fail "truncated values" [%message column.name];
      let bit_width = if present = 0 then 0 else Char.to_int page.[pos] in
      let indices, _ =
        if present = 0
        then [||], pos
        else
          Rle_hybrid.decode
            ~bit_width
            page
            ~pos:(pos + 1)
            ~stop:(String.length page)
            ~len:present
      in
      Array.map indices ~f:(fun i ->
        if i < 0 || i >= Array.length dictionary
        then fail "dictionary index out of range" [%message column.name (i : int)];
        dictionary.(i))
    | Rle when [%equal: M.Physical_type.t option] column.type_ (Some Boolean) ->
      let len = int32_le page pos in
      let bits, _ =
        Rle_hybrid.decode
          ~bit_width:1
          page
          ~pos:(pos + 4)
          ~stop:(pos + 4 + len)
          ~len:present
      in
      Array.map bits ~f:(fun b -> Value.Bool (b = 1))
    | encoding ->
      fail "unsupported encoding" [%message column.name (encoding : M.Encoding.t)]
  in
  match levels with
  | None -> values
  | Some levels ->
    let next = ref 0 in
    Array.map levels ~f:(fun l ->
      if l = 1
      then (
        let v = values.(!next) in
        incr next;
        v)
      else Value.Null)
;;

let read_chunk t (column : M.Schema_element.t) (chunk : M.Column_chunk.t) ~num_rows =
  let meta = chunk.meta_data in
  let start =
    match meta.dictionary_page_offset with
    | Some d when d > 0 -> Int.min d meta.data_page_offset
    | _ -> meta.data_page_offset
  in
  let bytes = read_at t.ic ~pos:start ~len:meta.total_compressed_size in
  let out = Array.create ~len:num_rows Value.Null in
  let filled = ref 0 in
  let dictionary = ref None in
  let pos = ref 0 in
  while !filled < num_rows do
    if !pos >= String.length bytes
    then fail "column chunk ends early" [%message column.name];
    let header, body_pos = Thrift_compact.read_struct bytes ~pos:!pos in
    let header = M.Page_header.of_thrift header in
    if header.compressed_page_size < 0
       || body_pos + header.compressed_page_size > String.length bytes
    then fail "page outside its column chunk" [%message column.name];
    let body = String.sub bytes ~pos:body_pos ~len:header.compressed_page_size in
    pos := body_pos + header.compressed_page_size;
    let page () =
      decompress meta.codec body ~uncompressed_size:header.uncompressed_page_size
    in
    match header.type_ with
    | Dictionary_page ->
      let h =
        match header.dictionary_page_header with
        | Some h -> h
        | None -> fail "dictionary page without a header" [%message column.name]
      in
      dictionary := Some (fst (decode_plain column (page ()) ~pos:0 ~n:h.num_values))
    | Data_page ->
      let h =
        match header.data_page_header with
        | Some h -> h
        | None -> fail "data page without a header" [%message column.name]
      in
      let values = decode_data_page column ~dictionary:!dictionary h (page ()) in
      if !filled + Array.length values > num_rows
      then fail "more values than rows" [%message column.name];
      Array.blit
        ~src:values
        ~src_pos:0
        ~dst:out
        ~dst_pos:!filled
        ~len:(Array.length values);
      filled := !filled + Array.length values
    | Data_page_v2 -> fail "data page v2 is not supported" [%message column.name]
    | Index_page -> ()
  done;
  out
;;

let row_group t i =
  match List.nth t.metadata.row_groups i with
  | Some rg -> rg
  | None -> fail "no such row group" [%message (i : int)]
;;

let column_index t name =
  match
    Array.findi t.columns ~f:(fun _ (c : M.Schema_element.t) -> String.equal c.name name)
  with
  | Some (i, _) -> i
  | None -> fail "no such column" [%message name]
;;

let read_column_exn t ~row_group:i index =
  let rg = row_group t i in
  match List.nth rg.columns index with
  | None -> fail "row group has fewer column chunks than the schema" [%message (i : int)]
  | Some chunk -> read_chunk t t.columns.(index) chunk ~num_rows:rg.num_rows
;;

let tag t = Or_error.tag ~tag:t.path

let read_column t ~row_group name =
  Or_error.try_with (fun () -> read_column_exn t ~row_group (column_index t name))
  |> tag t
;;

let read_row_group_exn t i =
  let columns =
    Array.mapi t.columns ~f:(fun index _ -> read_column_exn t ~row_group:i index)
  in
  let num_rows = (row_group t i).num_rows in
  Array.init num_rows ~f:(fun r -> Array.map columns ~f:(fun c -> c.(r)))
;;

let read_row_group t i = Or_error.try_with (fun () -> read_row_group_exn t i) |> tag t

let fold_rows t ~init ~f =
  Or_error.try_with (fun () ->
    List.foldi t.metadata.row_groups ~init ~f:(fun i acc _ ->
      Array.fold (read_row_group_exn t i) ~init:acc ~f))
  |> tag t
;;
