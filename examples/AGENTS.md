# AGENTS.md (examples/)

`trades.ml` writes a small trades file and is the worked example in the README. CI runs it with a
temporary output path, so it must write only to the path it is given. Keep it short; readers copy it.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @parquet/all`.
