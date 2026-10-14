# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[semantic versioning](https://semver.org/) (0.x: minor versions may break the API).

## Unreleased

### Added

- `Schema.Column.optional`: nullable columns, written with definition levels and counted
  in `null_count`; `Writer.Direct.add_null_int` and friends.
- `Schema.Column.dictionary`: dictionary pages and RLE/bit-packed indices, with a PLAIN
  fallback past 1 MiB of dictionary per chunk.
- `Writer.create ?key_value_metadata` (and `with_file`).
- `Reader`: reads flat files from this writer and others: v1 and v2 data pages,
  dictionary pages, PLAIN, dictionary, RLE, DELTA and BYTE_STREAM_SPLIT encodings,
  uncompressed, zstd, snappy and lz4_raw pages.
- `Thrift_compact.read_struct` / `of_string`, `Metadata.*.of_thrift`, the full
  thrift enums, `Dictionary_page_header`, `Data_page_header_v2` and `Key_value`.
- `Rle_hybrid`, `Delta` and `Decompress` (snappy, lz4_raw) codecs.
- `Byte_buf.add_bytes` and `Byte_buf.set_int32_le`.
- A top-level `Parquet` module doc and `examples/trades.ml`, run in CI.

### Changed

- `Metadata` records gained fields (`Page_header.dictionary_page_header`,
  `Page_header.data_page_header_v2`, `Column_meta_data.dictionary_page_offset`,
  `File_meta_data.key_value_metadata`) and enums gained constructors; code that builds
  these records or matches the enums exhaustively must be updated. `Schema` and
  `Writer` are source compatible, and files with only required, non-dictionary columns
  are byte-for-byte the same as before.
- The footer's `created_by` takes its version from `dune subst`.
- opam metadata: authors, maintainer, homepage, documentation link, description and
  dependency lower bounds.

## 0.1.0

### Added

- `Schema.Column` combinators for `int32`, `int64`, `float`, `utf8`, `bytes`,
  `timestamp_ns`, `decimal` (precision up to 18) and `bool` columns.
- `Writer`: streaming writer with row groups, one PLAIN-encoded v1 data page per column
  chunk, `null_count` and min/max statistics, type-defined column order, optional zstd
  compression.
- `Writer.Direct`: per-column appends without closures, `float#` entry point, zero minor
  words per row in steady state.
- `Thrift_compact` and `Metadata`: the Thrift compact protocol writer and the
  `parquet.thrift` structs the writer uses.
