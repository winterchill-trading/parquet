# AGENTS.md (bench/)

`bench.ml` is a throughput benchmark of writing and reading. The
README's numbers come from an Apple M-series laptop; quote the machine with any figure.

- Benchmarks are not pinned thresholds. The allocation pins live in `../test/test_alloc.ml`.
- On a loaded host, build the old and new binaries and run them alternately; take the best run.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/all`.
