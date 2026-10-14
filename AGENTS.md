# AGENTS.md

Guide for AI coding agents working in this repository. Read this first; the subdirectory
`AGENTS.md` files add detail for `lib/`, `test/`, `bench/`, `examples/`, `docs/` and `.github/`.
The workspace-wide guide is `../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`parquet` is a pure OCaml Apache Parquet writer and reader: Thrift compact protocol, PLAIN,
dictionary, RLE and bit-packed encodings, and (read-only) DELTA and BYTE_STREAM_SPLIT. Zstd goes
through a small C stub; snappy and lz4_raw are decompressed in OCaml. There is no Arrow and no C++.
It is the analytical layer that `market-data` writes and DuckDB queries.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The `parquet` library (`Parquet`): writer, reader, typed schema, encodings, metadata, zstd and decompression |
| `test/` | Expect tests, Quickcheck properties, DuckDB round trips (`duckdb.ml`), a frozen reference writer, allocation tests |
| `bench/` | Writer and reader benchmark |
| `examples/` | `trades.ml`, the worked example CI runs |
| `docs/` | `parquet.thrift`, the format definitions the metadata module follows |
| `.github/` | CI workflow |
| `dune-project` | Source of truth for package metadata; `dune build` regenerates `parquet.opam` |

## Build, test, docs

Inside the workspace (sibling checkouts and the shared `5.2.0+ox` switch are required):

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/all
opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/runtest
opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/doc
```

Parallel work: prefix the commands with `DUNE_BUILD_DIR=_build-parquet`.

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
then `opam install --deps-only --with-test .` and `dune build && dune test`. The tests need the DuckDB
CLI (v1.5.6 in CI) on the PATH; zstd needs libzstd.

## Conventions

- Every module has an `.mli`. Code is `open! Core`, labelled arguments, ppx_jane.
- DuckDB is the reader oracle. A file this writer produces must read back identically in DuckDB, and
  the writer must stay byte-equal to the frozen reference for required PLAIN columns.
- Allocation pins are part of the contract for `Writer.Direct`. Pin counts that differ between macOS
  and Linux as a range, with a comment that says why.
- Formatting: `opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/fmt --auto-promote`.
  Never run `dune fmt` at the workspace root.
- `CHANGES.md`: add a line under `## Unreleased` for every user-visible change.

## Dependencies and dependents

- Depends on `core` and `zstd` only; no org package.
- Used by `market-data`, `hl-leaderboard` and `hl-copy-core`.
- CI has no sibling pins.

## CI

`ci.yml` installs the DuckDB CLI, runs the `ox-setup` action, then `dune build`, `dune test`, runs
`examples/trades.exe` against a temporary file, builds odoc with warnings failing, and calls the
shared `obj-magic` workflow. Check a run with `gh run list --limit 1`, then at most one
`gh run watch <id> --exit-status`.

## Pitfalls

- `TIMESTAMP(NANOS, isAdjustedToUTC=true)` makes DuckDB read a time-zone column that drops
  nanoseconds. The default `isAdjustedToUTC=false` gives `TIMESTAMP_NS`.
- DuckDB rejects a whole file when any STRING value is invalid UTF-8; use `bytes` for opaque data.
- Without `ORDER BY`, rows across row groups come back in any order. Tests read with
  `file_row_number = true` and sort by it.
- A bare `dune promote --root .` once applied a stale pending promotion and restored an old
  `lib/writer.ml` over a rewrite. Promote one file: `dune promote <file>`.
- `(*` inside an OCaml comment opens a nested comment; `count(*)` in a comment breaks the build.
- A byte buffer with a mutable `bytes` field read through `Bytes.length` was 5x slower than
  `Stdlib.Buffer`; keep the layout of Buffer's storage.

## Where to read more

- `README.md`: writing, reading, supported types, known gaps, performance, portability, testing.
- `../specs/repos/parquet.md`, and `../specs/research/opam-readiness/parquet.md`.
- `../oxcaml-claude-skills/skills/oxcaml-performance/`.
- `../oxcaml-claude-skills/GOTCHAS.md`, section "Writing Parquet from pure OCaml".

## Never do

- Install or upgrade opam packages that change the compiler, `core`, `base`, `async` or
  `ppx_jane` on the shared switch. Run `opam install --switch 5.2.0+ox --dry-run <pkg>` first
  and stop if the plan touches any of them.
- Publish, tag or release, or add `pin-depends` or a forked copy of another package.
- Print secrets, or poll CI in a loop.
