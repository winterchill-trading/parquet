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
symbol, decimal price, double size, side, int64 id) on an Apple M-series
laptop:

| Rows       | Compression | Time    | Throughput     | File size |
|------------|-------------|---------|----------------|-----------|
| 1,000,000  | none        | 0.08 s  | 13 M rows/s    | 39.5 MB   |
| 1,000,000  | zstd (3)    | 0.11 s  | 9.4 M rows/s   | 5.2 MB    |
| 10,000,000 | none        | 1.02 s  | 9.8 M rows/s   | 395 MB    |
| 10,000,000 | zstd (3)    | 1.07 s  | 9.4 M rows/s   | 52 MB     |

Peak RSS for the 10M row run is about 220 MB: a whole row group is buffered
in memory before it is written.

## Testing

`dune test` runs expect tests for the Thrift encoder and metadata byte
layouts, and writes files that are read back with the DuckDB CLI (`duckdb`
must be on `PATH`), including a Quickcheck round trip over random rows, row
group sizes and compression.

## Layout

- `lib/thrift_compact.ml`: Thrift compact protocol writer.
- `lib/metadata.ml`: the parquet.thrift structs used (copy in `docs/`).
- `lib/encoding.ml`: PLAIN encoders.
- `lib/schema.ml`: typed column combinators.
- `lib/writer.ml`: row groups, pages, statistics, compression, footer.

## CI

CI installs the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.
