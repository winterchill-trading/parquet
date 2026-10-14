# AGENTS.md (lib/)

The `Parquet` library.

- Writing: `Writer` (`create`, `add`, `close`, `with_file`, `add_all`, row groups of 1,048,576 rows
  by default) and `Writer.Direct` (per-column adders, no per-cell closure). `Schema` gives the
  typed column combinators.
- Reading: `Reader` for flat files from any writer (v1 and v2 pages, dictionary, RLE, DELTA,
  BYTE_STREAM_SPLIT; uncompressed, zstd, snappy, lz4_raw).
- Format plumbing: `Thrift_compact`, `Metadata` (to and from the `parquet.thrift` structs),
  `Encoding`, `Rle_hybrid`, `Delta`, `Decompress`, `Zstd_bytes` (C stub over `ZSTD_compress`),
  `Byte_buf`.
- Timestamps default to `isAdjustedToUTC=false` so DuckDB keeps nanoseconds.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/all`.
