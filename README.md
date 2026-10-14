# parquet

Write and read [Apache Parquet](https://parquet.apache.org/) files from OCaml,
with no Arrow or C++ dependency. The Thrift compact protocol, the PLAIN,
dictionary and RLE/bit-packed encodings, definition levels for nullable
columns, statistics and the file footer are implemented in OCaml; zstd goes
through libzstd. The files are standard Parquet, and every feature is tested
against the DuckDB CLI.

The library targets [OxCaml](https://oxcaml.org) and the Jane Street libraries
(`core`, `ppx_jane`) from its opam repository; see [Portability](#portability).

## Writing

Describe the columns of a row type with `Schema.Column` combinators, then
stream rows into a file:

```ocaml
open! Core

type trade =
  { ts : Time_ns.t
  ; coin : string
  ; px : int (* unscaled, 6 decimals *)
  ; sz : float
  ; is_buy : bool
  ; fee : int option
  }

let schema =
  Parquet.Schema.create
    Parquet.Schema.Column.
      [ timestamp_ns "ts" (fun t -> t.ts)
      ; dictionary (utf8 "coin" (fun t -> t.coin))
      ; decimal ~precision:18 ~scale:6 "px" (fun t -> t.px)
      ; float "sz" (fun t -> t.sz)
      ; bool "is_buy" (fun t -> t.is_buy)
      ; optional (decimal ~precision:18 ~scale:6) "fee" (fun t -> t.fee)
      ]
;;

let write trades =
  Parquet.Writer.with_file ~schema ~compression:`Zstd "trades.parquet" ~f:(fun w ->
    Parquet.Writer.add_all w trades)
;;
```

```
$ duckdb -c "describe select * from 'trades.parquet'"
ts      TIMESTAMP_NS
coin    VARCHAR
px      DECIMAL(18,6)
sz      DOUBLE
is_buy  BOOLEAN
fee     DECIMAL(18,6)
```

[`examples/trades.ml`](examples/trades.ml) is a complete program that writes
such a file and reads it back; CI builds and runs it.

`Writer.create` / `Writer.add` / `Writer.close` are available for streaming;
`row_group_rows` (default 1_048_576) controls when a row group is flushed, and
`key_value_metadata` adds application key-value pairs to the footer.

- `Column.optional make name f` makes a nullable column: `f` returns an
  option, `None` is written as a null (definition level 0) and counted in the
  `null_count` statistic.
- `Column.dictionary column` dictionary-encodes a column: each column chunk
  gets a dictionary page and the data page holds RLE/bit-packed indices. Use
  it for low-cardinality values (symbols, venues, enums). A chunk whose
  dictionary passes 1 MiB is written PLAIN instead.

On a hot path, `Writer.Direct` appends column values without the schema
accessors: no closure call per cell, and `add_float_u` takes an unboxed
`float#`. Look the columns up once, give every column one value per row (or
`add_null_*` for an optional column), then call `end_row` (it raises if a
column got none or two). The file is the same as `Writer.add` writes, and both
can be mixed.

```ocaml
let ts = Parquet.Writer.Direct.int w "ts"
and sz = Parquet.Writer.Direct.float w "sz" in
(* per row, one call per column: *)
Parquet.Writer.Direct.add_int ts (Time_ns.to_int_ns_since_epoch trade.ts);
Parquet.Writer.Direct.add_float sz trade.sz;
Parquet.Writer.Direct.end_row w
```

## Reading

`Reader` reads files with a flat schema (every column a leaf under the root,
required or optional), from this library or from other writers:

```ocaml
let rows =
  Parquet.Reader.with_file "trades.parquet" ~f:(fun r ->
    Parquet.Reader.fold_rows r ~init:[] ~f:(fun acc row -> row :: acc))
;;
```

Values come back as `Reader.Value.t` (`Null`, `Bool`, `Int`, `Float`,
`String`), a row as an array in column order; `Reader.read_column` reads one
column chunk, `Reader.columns` and `Reader.metadata` give the schema and the
footer. It handles v1 and v2 data pages, dictionary pages, several pages per
chunk, the PLAIN, dictionary, RLE, DELTA_BINARY_PACKED,
DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY and BYTE_STREAM_SPLIT encodings, and
uncompressed, zstd, snappy and lz4_raw pages. Errors (unsupported features,
malformed input) come back as `Or_error.t`; nothing is read out of bounds.

## Supported types

| Combinator      | Physical type | Logical type                    | OCaml value |
|-----------------|---------------|---------------------------------|-------------|
| `int32`         | INT32         |                                 | `int` (range-checked) |
| `int64`         | INT64         |                                 | `int` (63 bits) |
| `float`         | DOUBLE        |                                 | `float` |
| `utf8`          | BYTE_ARRAY    | STRING                          | `string` (must be UTF-8) |
| `bytes`         | BYTE_ARRAY    |                                 | `string` |
| `timestamp_ns`  | INT64         | TIMESTAMP(NANOS)                | `Time_ns.t` |
| `decimal`       | INT64         | DECIMAL(precision <= 18, scale) | `int`, unscaled |
| `bool`          | BOOLEAN       |                                 | `bool` |

Any of them can be wrapped in `optional`, and all but `bool` in `dictionary`.
Every column chunk is one v1 data page (after a dictionary page for dictionary
columns), optionally zstd-compressed. Column chunks and pages carry
`null_count` and `min_value`/`max_value` statistics for integer, decimal,
timestamp, double and byte array columns (NaN is excluded), and the footer
declares type-defined column order so readers trust them.

`timestamp_ns` writes `isAdjustedToUTC=false` by default. DuckDB maps
`isAdjustedToUTC=true` to `TIMESTAMP WITH TIME ZONE`, which has microsecond
resolution and silently truncates the nanoseconds; pass
`~adjusted_to_utc:true` if your reader handles it.

## Known gaps

Left out on purpose, each needing a design decision or an outside dependency:

- Nested and repeated columns (lists, maps, structs). Repetition levels are
  mechanical, but the typed schema API has no way yet to describe nesting over
  an OCaml row type; that design comes first. The reader rejects such files.
- Writing more than one page per column chunk. A row group's column must fit
  in one page (under 2 GiB); lower `row_group_rows` to stay under it. Splitting
  pages needs row boundaries and per-page statistics in every column builder,
  a cost on the hot path that should be measured against a real need first.
- Writing DELTA, BYTE_STREAM_SPLIT or v2 pages, page indexes and bloom
  filters. The reader decodes the encodings and v2 pages; which writer
  features are worth their cost is a product decision.
- The gzip, brotli and legacy (framed) lz4 codecs, in either direction, and
  snappy or lz4 compression when writing: they need new C bindings or a large
  pure OCaml port. zstd covers writing; the reader decompresses snappy and
  lz4_raw in OCaml.
- Encryption: needs a crypto dependency and a key management design.
- INT96 timestamps (deprecated by the format) are rejected by the reader.
- The reader loads a whole column chunk into memory and builds a boxed value
  per cell; a typed, streaming column reader is a separate API to design.

## Performance

`dune exec bench/bench.exe -- 1000000` writes trade-like rows (timestamp,
symbol, decimal price, double size, side, int64 id) through `Writer.add` (one
record per row, schema accessors) and through `Writer.Direct`. Apple M-series
laptop (arm64), `--profile release`, best of several runs interleaved with the
previous writer; CI runs on amd64 and is not benchmarked.

| Rows       | Path              | Compression | Before        | After         |
|------------|-------------------|-------------|---------------|---------------|
| 1,000,000  | `add`             | none        | 13.5 M rows/s | 16.1 M rows/s |
| 1,000,000  | `add`             | zstd (3)    | 9.4 M rows/s  | 10.6 M rows/s |
| 1,000,000  | `Direct`          | none        |               | 20.4 M rows/s |
| 1,000,000  | `Direct`          | zstd (3)    |               | 11.5 M rows/s |
| 10,000,000 | `add`             | none        | 15.1 M rows/s | 18.5 M rows/s |
| 10,000,000 | `add`             | zstd (3)    | 9.7 M rows/s  | 11.0 M rows/s |
| 10,000,000 | `Direct`          | none        |               | 20.7 M rows/s |
| 10,000,000 | `Direct`          | zstd (3)    |               | 11.8 M rows/s |

Dev profile: 13.2 to 14.1 (`add`) and 14.5 (`Direct`) M rows/s without
compression; `-O3`: 14.5 to 16.4 and 23.8. Files are 39.5 MB (none) and
5.2 MB (zstd) per million rows. With zstd, compression is most of the time.

Allocation per row, pinned in `test/test_alloc.ml`: `Writer.add` and
`Writer.Direct` allocate 0 minor words per row in steady state (`Direct`
with a float computed per row, through `add_float_u`). Over the 10M row bench
the writer allocates about 1.1 words per row, down from 6 to 6.6, all of it
column buffers growing in the first row group: they are reused afterwards, and
pages are compressed and written straight from them (`Zstd_bytes`, a small C
stub over `ZSTD_compress`) instead of copied into strings twice.

Peak RSS for the 10M row bench went from 220 MB to 128 MB: a whole row group
is still buffered in memory before it is written, but no longer copied.

Optional and dictionary columns sit behind one flag test per value, so plain
columns keep the numbers above (interleaved runs of the writer before and after
they were added differ by 2 to 3%, within run-to-run noise on a laptop).
`Writer.add` with optional columns also allocates 0 words per row
(`test/test_alloc.ml`).

The bench reads every `add` file back through `Reader.fold_rows`: about
3 M rows/s (none or zstd) and 55 to 59 words per row, since the reader builds
a `Value.t` per cell and an array per row. It is meant for checking and
small exports, not as a scan engine.

## Portability

OxCaml-only constructs, all in the writer's hot path:

| Construct | Where | Plain OCaml 5.x |
|-----------|-------|-----------------|
| `float#` (unboxed float) | `Writer.Direct.add_float_u`, `Byte_buf.add_double_u_le` | remove the two `_u` functions |
| `%box_float` / `%unbox_float` externals | `lib/writer.ml`, `lib/byte_buf.ml` | not needed once the above go |

Nothing else is OxCaml-specific: no modes, `or_null`, `[@zero_alloc]` or
unboxed records. A plain OCaml 5.2 build needs those two functions behind a
build-time switch (for example a dune `select` between two `.ml` files), and
`core`, `ppx_jane` and `core_unix` at a released upstream version instead of
the `v0.18~preview` packages, which exist only in the OxCaml opam repository.
The zstd C stub (`lib/parquet_zstd_stubs.c`) is plain C against libzstd.

## Testing

`dune test` runs expect tests for the Thrift codec, the metadata structs and
the RLE and delta codecs, and writes files that are read back with the DuckDB
CLI (`duckdb` must be on `PATH`), including Quickcheck round trips over random
rows with nulls and dictionaries, row group sizes and compression. The reader
is checked two ways: it must return what the writer wrote, and on files
DuckDB writes (v1 and v2 pages, every codec it offers but gzip and brotli) it
must print exactly what DuckDB prints. DuckDB does not write
DELTA_BYTE_ARRAY, so `test/fixtures/` holds a small pyarrow file and the
script that made it. `test/reference_writer.ml` is a frozen copy of the
original writer; a Quickcheck test checks that `Writer.add` and
`Writer.Direct` write byte-for-byte the same files as it does for required,
PLAIN columns.

## Layout

- `lib/thrift_compact.ml`: Thrift compact protocol writer and reader.
- `lib/metadata.ml`: the parquet.thrift structs (copy in `docs/`), both ways.
- `lib/encoding.ml`: PLAIN encoders.
- `lib/rle_hybrid.ml`: RLE/bit-packing hybrid encoder and decoder.
- `lib/schema.ml`: typed column combinators.
- `lib/byte_buf.ml`: growable column buffer with direct access to its bytes.
- `lib/zstd_bytes.ml`, `lib/parquet_zstd_stubs.c`: zstd from and into bytes.
- `lib/writer.ml`: typed column builders, definition levels, dictionaries, row
  groups, pages, statistics, compression, footer; `Writer.Direct`.
- `lib/reader.ml`, `lib/delta.ml`, `lib/decompress.ml`: the reader, the
  format 2 decoders, snappy and lz4_raw.
- `examples/trades.ml`: the worked example.

## Releasing

Not released yet; the plan once the dependencies are on a public opam
repository:

1. Move the `## Unreleased` entries of `CHANGES.md` under a version heading
   (`0.1.0` first).
2. `dune-release tag` to create the `v<version>` tag from that heading.
3. `dune-release distrib`, then `dune-release publish` and
   `dune-release opam pkg` / `opam submit` to the opam repository that holds
   the OxCaml packages.

The version lives only in `CHANGES.md` and the tag: `dune subst` fills in the
`created_by` string the writer puts in the footer.

## CI

CI builds on the OxCaml switch (cached between runs), installs the DuckDB CLI
v1.5.6 as the test oracle, runs `dune test` and runs the worked example.

## Used by

`market-data` (Parquet sinks), `hl-leaderboard` and `hl-copy-core`.
