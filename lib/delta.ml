open! Core

let[@cold] malformed what ~pos =
  raise_s [%message "Parquet.Delta: malformed input" (what : string) (pos : int)]
;;

let binary_packed s ~pos ~stop =
  if pos < 0 || stop > String.length s || pos > stop then malformed "bounds" ~pos;
  let p = ref pos in
  let byte () =
    if !p >= stop then malformed "truncated" ~pos:!p;
    let b = Char.to_int (String.unsafe_get s !p) in
    incr p;
    b
  in
  let uvarint64 () =
    let rec go shift acc =
      if shift > 63 then malformed "varint too long" ~pos:!p;
      let b = byte () in
      let acc = Int64.bit_or acc (Int64.shift_left (Int64.of_int (b land 0x7F)) shift) in
      if b land 0x80 = 0 then acc else go (shift + 7) acc
    in
    go 0 0L
  in
  let uvarint what =
    match Int64.to_int (uvarint64 ()) with
    | Some n when n >= 0 && n < 1 lsl 40 -> n
    | _ -> malformed what ~pos:!p
  in
  let svarint64 () =
    let n = uvarint64 () in
    Int64.bit_xor (Int64.shift_right_logical n 1) (Int64.neg (Int64.bit_and n 1L))
  in
  let block_size = uvarint "block size" in
  let miniblocks = uvarint "miniblock count" in
  let total = uvarint "value count" in
  let first = svarint64 () in
  if miniblocks = 0
     || block_size = 0
     || block_size > 1 lsl 24
     || block_size % miniblocks <> 0
  then malformed "block layout" ~pos:!p;
  let per_miniblock = block_size / miniblocks in
  if per_miniblock % 8 <> 0 then malformed "miniblock size" ~pos:!p;
  (* Every block takes at least one byte, so a count beyond that is malformed; this bounds
     the allocation below. *)
  if total > 1 && total - 1 > (stop - !p) * block_size
  then malformed "value count" ~pos:!p;
  let out = Array.create ~len:total 0L in
  if total > 0 then out.(0) <- first;
  let filled = ref 1 in
  let last = ref first in
  (* [w] bits starting [bit] bits after byte [start], least significant bit first. *)
  let read_bits ~start ~bit ~w =
    let v = ref 0L
    and got = ref 0
    and off = ref bit in
    while !got < w do
      let b = Char.to_int (String.unsafe_get s (start + (!off / 8))) in
      let shift = !off % 8 in
      let take = Int.min (8 - shift) (w - !got) in
      let bits = (b lsr shift) land ((1 lsl take) - 1) in
      v := Int64.bit_or !v (Int64.shift_left (Int64.of_int bits) !got);
      got := !got + take;
      off := !off + take
    done;
    !v
  in
  while !filled < total do
    let min_delta = svarint64 () in
    let widths = Array.init miniblocks ~f:(fun _ -> byte ()) in
    Array.iter widths ~f:(fun w ->
      if !filled < total
      then (
        if w > 64 then malformed "bit width" ~pos:!p;
        let bytes = per_miniblock * w / 8 in
        if bytes > stop - !p then malformed "truncated miniblock" ~pos:!p;
        let start = !p in
        for j = 0 to per_miniblock - 1 do
          if !filled < total
          then (
            let delta = read_bits ~start ~bit:(j * w) ~w in
            last := Int64.( + ) !last (Int64.( + ) min_delta delta);
            out.(!filled) <- !last;
            incr filled)
        done;
        p := !p + bytes))
  done;
  out, !p
;;

let length_byte_array s ~pos ~stop =
  let lengths, p = binary_packed s ~pos ~stop in
  let p = ref p in
  let values =
    Array.map lengths ~f:(fun len ->
      match Int64.to_int len with
      | Some len when len >= 0 && len <= stop - !p ->
        let v = String.sub s ~pos:!p ~len in
        p := !p + len;
        v
      | _ -> malformed "byte array length" ~pos:!p)
  in
  values, !p
;;

let byte_array s ~pos ~stop =
  let prefixes, p = binary_packed s ~pos ~stop in
  let suffixes, p = length_byte_array s ~pos:p ~stop in
  if Array.length prefixes <> Array.length suffixes
  then malformed "prefix and suffix counts differ" ~pos:p;
  let previous = ref "" in
  let values =
    Array.mapi suffixes ~f:(fun i suffix ->
      match Int64.to_int prefixes.(i) with
      | Some len when len >= 0 && len <= String.length !previous ->
        let v = String.prefix !previous len ^ suffix in
        previous := v;
        v
      | _ -> malformed "prefix length" ~pos:p)
  in
  values, p
;;

let byte_stream_split s ~pos ~n ~width =
  if n < 0 || width <= 0 || pos < 0 || n * width > String.length s - pos
  then malformed "truncated byte stream split" ~pos;
  String.init (n * width) ~f:(fun k ->
    let i = k / width
    and j = k % width in
    s.[pos + (j * n) + i])
;;
