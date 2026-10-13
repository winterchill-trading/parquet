# parquet

A pure OCaml writer for [Apache Parquet](https://parquet.apache.org/) files,
built with OxCaml and the Jane Street standard library. No Arrow, no C++:
the Thrift compact protocol, PLAIN encoding and footer are implemented here.
Zstandard compression goes through the `zstd` opam bindings.

## Usage

```ocaml
open! Core

type trade =
  { ts : Time_ns.t
  ; coin : string
  ; px : int (* unscaled, 6 decimals *)
  ; sz : float
  ; is_buy : bool
  }

let schema =
  Parquet.Schema.create
    Parquet.Schema.Column.
      [ timestamp_ns "ts" (fun t -> t.ts)
      ; utf8 "coin" (fun t -> t.coin)
      ; decimal ~precision:18 ~scale:6 "px" (fun t -> t.px)
      ; float "sz" (fun t -> t.sz)
      ; bool "is_buy" (fun t -> t.is_buy)
      ]
;;

let write trades =
  Parquet.Writer.with_file ~schema ~compression:`Zstd "trades.parquet" ~f:(fun w ->
    Parquet.Writer.add_all w trades)
;;
```

`Writer.create` / `Writer.add` / `Writer.close` are available for streaming;
`row_group_rows` (default 1_048_576) controls when a row group is flushed.

On a hot path, `Writer.Direct` appends column values without the schema
accessors: no closure call per cell, and `add_float_u` takes an unboxed
`float#`. Look the columns up once, give every column one value per row, then
call `end_row` (it raises if a column got none or two). The file is the same as
`Writer.add` writes, and both can be mixed.

```ocaml
let ts = Parquet.Writer.Direct.int w "ts"
and sz = Parquet.Writer.Direct.float w "sz" in
(* per row, one call per column: *)
Parquet.Writer.Direct.add_int ts (Time_ns.to_int_ns_since_epoch trade.ts);
Parquet.Writer.Direct.add_float sz trade.sz;
Parquet.Writer.Direct.end_row w
```

```
$ duckdb -c "describe select * from 'trades.parquet'"
ts      TIMESTAMP_NS
coin    VARCHAR
px      DECIMAL(18,6)
sz      DOUBLE
is_buy  BOOLEAN
```

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

Every column chunk is one v1 data page, PLAIN encoded, optionally
zstd-compressed. Column chunks and pages carry `null_count` and
`min_value`/`max_value` statistics for integer, decimal, timestamp, double and
byte array columns (NaN is excluded), and the footer declares type-defined
column order so readers trust them.

`timestamp_ns` writes `isAdjustedToUTC=false` by default. DuckDB maps
`isAdjustedToUTC=true` to `TIMESTAMP WITH TIME ZONE`, which has microsecond
resolution and silently truncates the nanoseconds; pass
`~adjusted_to_utc:true` if your reader handles it.

## Not supported

- Nullable (`OPTIONAL`) and nested/repeated columns; no definition or
  repetition levels are written.
- Dictionary, RLE, delta and byte-stream-split encodings; only PLAIN.
- Data page v2, page indexes, bloom filters, key-value metadata, encryption.
- Compression codecs other than zstd (no snappy, gzip, lz4).
- Splitting a column chunk into several pages: a row group's column must fit in
  one page (under 2 GiB).
- Reading Parquet files.

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

## Testing

`dune test` runs expect tests for the Thrift encoder and metadata byte
layouts, and writes files that are read back with the DuckDB CLI (`duckdb`
must be on `PATH`), including a Quickcheck round trip over random rows, row
group sizes and compression. `test/reference_writer.ml` is a frozen copy of
the original writer; a Quickcheck test checks that `Writer.add` and
`Writer.Direct` write byte-for-byte the same files as it does.

## Layout

- `lib/thrift_compact.ml`: Thrift compact protocol writer.
- `lib/metadata.ml`: the parquet.thrift structs used (copy in `docs/`).
- `lib/encoding.ml`: PLAIN encoders.
- `lib/schema.ml`: typed column combinators.
- `lib/byte_buf.ml`: growable column buffer with direct access to its bytes.
- `lib/zstd_bytes.ml`, `lib/parquet_zstd_stubs.c`: zstd from and into bytes.
- `lib/writer.ml`: typed column builders, row groups, pages, statistics,
  compression, footer; `Writer.Direct`.

## CI

CI installs the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.
