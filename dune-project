(lang dune 3.17)
(name parquet)
(generate_opam_files true)
(license MIT)
(maintainers "winterchill-trading")
(authors "winterchill-trading")
(source (github winterchill-trading/parquet))
(package
 (name parquet)
 (synopsis "Pure OCaml Apache Parquet writer")
 (depends
  (ocaml (>= 5.2))
  dune
  core
  ppx_jane
  zstd))
