# AGENTS.md (test/)

Expect tests, Quickcheck properties, DuckDB round trips and allocation pins.

- `duckdb.ml` runs the DuckDB CLI on a file and returns its rows. The `test_duckdb_*` files are the
  oracle tests; a new writer feature needs one.
- `reference_writer.ml` is a frozen copy of an earlier writer; the tests compare against it so
  output stays byte-equal for required PLAIN columns. Do not edit the reference.
- `test_alloc.ml` pins words per row for `Writer.Direct`; a pin that moves needs the comment next to
  it explained.
- `fixtures/` holds a DELTA_BYTE_ARRAY file and the script that made it; regenerate only through that
  script.
- Promote expect output with `dune promote <file>` for the one file that changed.

Run: `opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/runtest`.
