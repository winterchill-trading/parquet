open! Core

let[@cold] malformed codec what ~pos =
  raise_s
    [%message
      "Parquet.Decompress: malformed input" (codec : string) (what : string) (pos : int)]
;;

(* Copies [len] bytes from [dist] bytes back in [out]; the ranges may overlap, which
   repeats the last [dist] bytes. *)
let copy_back out ~at ~dist ~len =
  for k = 0 to len - 1 do
    Bytes.unsafe_set out (at + k) (Bytes.unsafe_get out (at + k - dist))
  done
;;

let snappy s =
  let malformed = malformed "snappy" in
  let n = String.length s in
  let p = ref 0 in
  let byte () =
    if !p >= n then malformed "truncated" ~pos:!p;
    let b = Char.to_int (String.unsafe_get s !p) in
    incr p;
    b
  in
  let rec uvarint shift acc =
    if shift > 28 then malformed "length varint too long" ~pos:!p;
    let b = byte () in
    let acc = acc lor ((b land 0x7F) lsl shift) in
    if b land 0x80 = 0 then acc else uvarint (shift + 7) acc
  in
  let size = uvarint 0 0 in
  let out = Bytes.create size in
  let o = ref 0 in
  let le k =
    let v = ref 0 in
    for i = 0 to k - 1 do
      v := !v lor (byte () lsl (8 * i))
    done;
    !v
  in
  let copy ~len ~dist =
    if dist <= 0 || dist > !o then malformed "copy offset" ~pos:!p;
    if len > size - !o then malformed "copy past the declared size" ~pos:!p;
    copy_back out ~at:!o ~dist ~len;
    o := !o + len
  in
  while !p < n do
    let tag = byte () in
    match tag land 3 with
    | 0 ->
      let len =
        match tag lsr 2 with
        | l when l < 60 -> l + 1
        | l -> le (l - 59) + 1
      in
      if len > n - !p then malformed "literal past the input" ~pos:!p;
      if len > size - !o then malformed "literal past the declared size" ~pos:!p;
      Bytes.From_string.blit ~src:s ~src_pos:!p ~dst:out ~dst_pos:!o ~len;
      p := !p + len;
      o := !o + len
    | 1 ->
      let len = 4 + ((tag lsr 2) land 7) in
      let dist = ((tag lsr 5) lsl 8) lor byte () in
      copy ~len ~dist
    | 2 ->
      let len = (tag lsr 2) + 1 in
      copy ~len ~dist:(le 2)
    | _ ->
      let len = (tag lsr 2) + 1 in
      copy ~len ~dist:(le 4)
  done;
  if !o <> size then malformed "output shorter than the declared size" ~pos:!p;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:out
;;

let lz4_raw s ~uncompressed_size =
  let malformed = malformed "lz4_raw" in
  let n = String.length s in
  let size = uncompressed_size in
  if size < 0 then malformed "negative size" ~pos:0;
  let out = Bytes.create size in
  let p = ref 0 in
  let o = ref 0 in
  let byte () =
    if !p >= n then malformed "truncated" ~pos:!p;
    let b = Char.to_int (String.unsafe_get s !p) in
    incr p;
    b
  in
  (* A 4-bit length of 15 continues in bytes of 255 until a smaller one. *)
  let length nibble =
    if nibble < 15
    then nibble
    else (
      let rec more acc =
        let b = byte () in
        if b = 255 then more (acc + 255) else acc + b
      in
      more nibble)
  in
  let finished = ref (n = 0) in
  while not !finished do
    let token = byte () in
    let literals = length (token lsr 4) in
    if literals > n - !p then malformed "literal past the input" ~pos:!p;
    if literals > size - !o then malformed "literal past the declared size" ~pos:!p;
    Bytes.From_string.blit ~src:s ~src_pos:!p ~dst:out ~dst_pos:!o ~len:literals;
    p := !p + literals;
    o := !o + literals;
    (* The last sequence has literals only. *)
    if !p = n
    then finished := true
    else (
      let lo = byte () in
      let dist = lo lor (byte () lsl 8) in
      let len = length (token land 15) + 4 in
      if dist = 0 || dist > !o then malformed "match offset" ~pos:!p;
      if len > size - !o then malformed "match past the declared size" ~pos:!p;
      copy_back out ~at:!o ~dist ~len;
      o := !o + len)
  done;
  if !o <> size then malformed "output shorter than the declared size" ~pos:!p;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:out
;;
