# Changelog

All notable changes to this project are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and the project uses
[semantic versioning](https://semver.org/) (0.x: minor versions may break the API).

## Unreleased

### Changed

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
