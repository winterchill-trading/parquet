(** The RLE / bit-packing hybrid encoding Parquet uses for definition levels, dictionary
    indices and RLE booleans.

    The stream is a sequence of runs. An RLE run is a varint header [count lsl 1] followed
    by the repeated value in [ceil (bit_width / 8)] little-endian bytes. A bit-packed run
    is a varint header [(groups lsl 1) lor 1] followed by [groups * 8] values packed
    [bit_width] bits each, least significant bit first. Neither form carries the total
    count: the reader knows it from the page header. *)

open! Core

(** Smallest bit width that holds every value in [0, max_value]; at least 1. *)
val bit_width : max_value:int -> int

(** [encode ~bit_width ~len ~get buf] appends the encoding of [get 0], ...,
    [get (len - 1)] to [buf], with no length prefix. Runs of 8 or more equal values become
    RLE runs, the rest bit-packed runs; the last bit-packed group is padded with zeros.

    Raises if [bit_width] is outside [1, 32] or a value does not fit in it. *)
val encode : bit_width:int -> len:int -> get:(int -> int) -> Byte_buf.t -> unit

(** [decode ~bit_width s ~pos ~stop ~len] reads [len] values from the bytes of [s] between
    [pos] (inclusive) and [stop] (exclusive). Returns the values and the position after
    the last run read.

    Raises if the stream ends before [len] values or a run is malformed. *)
val decode : bit_width:int -> string -> pos:int -> stop:int -> len:int -> int array * int
