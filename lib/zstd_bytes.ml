open! Core

external compress_bound : int -> int = "parquet_zstd_compress_bound"

external compress_raw
  :  int
  -> Bytes.t
  -> int
  -> Bytes.t
  -> int
  = "parquet_zstd_compress"

let compress ~level src ~len ~dst =
  if len < 0 || len > Bytes.length src
  then raise_s [%message "Parquet.Zstd_bytes.compress: bad length" (len : int)];
  compress_raw level src len dst
;;
