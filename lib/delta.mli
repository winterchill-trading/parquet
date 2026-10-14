(** Decoders for the DELTA encodings and BYTE_STREAM_SPLIT, which Parquet format 2 writers
    use (DuckDB with [parquet_version v2], Arrow with [data_page_version 2.0]). The writer
    does not produce them.

    Every function reads from [s] between [pos] and [stop] and raises on malformed or
    truncated input. *)

open! Core

(** DELTA_BINARY_PACKED: a header (block size, miniblocks per block, value count, first
    value), then blocks of a minimum delta, miniblock bit widths and bit-packed deltas.
    Returns the values, computed with 64-bit wrapping arithmetic as the format requires,
    and the position after the last miniblock read. *)
val binary_packed : string -> pos:int -> stop:int -> int64 array * int

(** DELTA_LENGTH_BYTE_ARRAY: the lengths, DELTA_BINARY_PACKED, then the bytes of every
    value back to back. Returns the values and the position after them. *)
val length_byte_array : string -> pos:int -> stop:int -> string array * int

(** DELTA_BYTE_ARRAY: prefix lengths (DELTA_BINARY_PACKED) then suffixes
    (DELTA_LENGTH_BYTE_ARRAY); each value is that many bytes of the previous value
    followed by its suffix. *)
val byte_array : string -> pos:int -> stop:int -> string array * int

(** BYTE_STREAM_SPLIT for [n] values of [width] bytes: byte [j] of value [i] is at
    [pos + (j * n) + i]. Returns the values re-interleaved as in PLAIN encoding. *)
val byte_stream_split : string -> pos:int -> n:int -> width:int -> string
