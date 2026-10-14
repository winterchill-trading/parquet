open! Core

let bit_width ~max_value =
  let rec go w = if w >= 63 || max_value lsr w = 0 then w else go (w + 1) in
  Int.max 1 (go 0)
;;

let add_uvarint buf n =
  let n = ref n in
  while !n >= 0x80 do
    Byte_buf.add_char buf (Char.unsafe_of_int (!n land 0x7F lor 0x80));
    n := !n lsr 7
  done;
  Byte_buf.add_char buf (Char.unsafe_of_int !n)
;;

(* Length of the run of values equal to [get i] starting at [i], counting at most [limit]. *)
let run_length ~len ~get i ~limit =
  let v = get i in
  let j = ref (i + 1) in
  while !j < len && !j - i < limit && get !j = v do
    incr j
  done;
  !j - i
;;

let encode ~bit_width ~len ~get buf =
  if bit_width < 1 || bit_width > 32
  then raise_s [%message "Parquet.Rle_hybrid.encode: bit width" (bit_width : int)];
  let byte_width = (bit_width + 7) / 8 in
  let check v =
    if v < 0 || v lsr bit_width <> 0
    then
      raise_s
        [%message
          "Parquet.Rle_hybrid.encode: value does not fit" (v : int) (bit_width : int)];
    v
  in
  let i = ref 0 in
  while !i < len do
    let run = run_length ~len ~get !i ~limit:Int.max_value in
    if run >= 8
    then (
      let v = check (get !i) in
      add_uvarint buf (run lsl 1);
      for k = 0 to byte_width - 1 do
        Byte_buf.add_char buf (Char.unsafe_of_int ((v lsr (8 * k)) land 0xFF))
      done;
      i := !i + run)
    else (
      (* Bit-packed groups of 8 until the next group starts a run worth RLE-encoding. *)
      let start = !i in
      let groups = ref 0 in
      let continue = ref true in
      while !continue do
        i := !i + 8;
        incr groups;
        if !i >= len || run_length ~len ~get !i ~limit:8 >= 8 then continue := false
      done;
      add_uvarint buf ((!groups lsl 1) lor 1);
      let acc = ref 0
      and bits = ref 0 in
      for k = start to start + (!groups * 8) - 1 do
        let v = if k < len then check (get k) else 0 in
        acc := !acc lor (v lsl !bits);
        bits := !bits + bit_width;
        while !bits >= 8 do
          Byte_buf.add_char buf (Char.unsafe_of_int (!acc land 0xFF));
          acc := !acc lsr 8;
          bits := !bits - 8
        done
      done;
      i := Int.min !i len)
  done
;;

let[@cold] malformed what ~pos =
  raise_s
    [%message "Parquet.Rle_hybrid.decode: malformed stream" (what : string) (pos : int)]
;;

let decode ~bit_width s ~pos ~stop ~len =
  if bit_width < 0 || bit_width > 32
  then raise_s [%message "Parquet.Rle_hybrid.decode: bit width" (bit_width : int)];
  if pos < 0 || stop > String.length s || pos > stop then malformed "bounds" ~pos;
  let byte_width = (bit_width + 7) / 8 in
  let out = Array.create ~len 0 in
  let filled = ref 0 in
  let p = ref pos in
  let byte () =
    if !p >= stop then malformed "truncated" ~pos:!p;
    let b = Char.to_int (String.unsafe_get s !p) in
    incr p;
    b
  in
  let uvarint () =
    let rec go shift acc =
      if shift > 56 then malformed "varint too long" ~pos:!p;
      let b = byte () in
      let acc = acc lor ((b land 0x7F) lsl shift) in
      if b land 0x80 = 0 then acc else go (shift + 7) acc
    in
    go 0 0
  in
  while !filled < len do
    let header = uvarint () in
    if header land 1 = 0
    then (
      let count = header lsr 1 in
      if count = 0 then malformed "empty run" ~pos:!p;
      let v = ref 0 in
      for k = 0 to byte_width - 1 do
        v := !v lor (byte () lsl (8 * k))
      done;
      let n = Int.min count (len - !filled) in
      Array.fill out ~pos:!filled ~len:n !v;
      filled := !filled + n)
    else (
      let count = (header lsr 1) * 8 in
      let nbytes = (header lsr 1) * bit_width in
      if nbytes > stop - !p then malformed "truncated bit-packed run" ~pos:!p;
      let mask = (1 lsl bit_width) - 1 in
      let acc = ref 0
      and bits = ref 0 in
      for _ = 1 to count do
        while !bits < bit_width do
          acc := !acc lor (byte () lsl !bits);
          bits := !bits + 8
        done;
        if !filled < len
        then (
          out.(!filled) <- !acc land mask;
          incr filled);
        acc := !acc lsr bit_width;
        bits := !bits - bit_width
      done)
  done;
  out, !p
;;
