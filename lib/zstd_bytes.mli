(** Zstandard compression from and into [Bytes.t] buffers, with no intermediate string.
    Produces the same frames as [Zstd.compress] at the same level. *)

open! Core

(** Upper bound on the compressed size of [len] bytes. *)
val compress_bound : int -> int

(** [compress ~level src ~len ~dst] compresses the first [len] bytes of [src] into [dst]
    and returns the compressed length. [dst] must hold [compress_bound len] bytes. Raises
    on a zstd error. *)
val compress : level:int -> Bytes.t -> len:int -> dst:Bytes.t -> int
