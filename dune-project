(lang dune 3.17)

(name parquet)

(generate_opam_files true)

(license MIT)

(authors "WinterChill AI")

(maintainers "hello@winterchill.xyz")

(source
 (github winterchill-trading/parquet))

(homepage "https://github.com/winterchill-trading/parquet")

(bug_reports "https://github.com/winterchill-trading/parquet/issues")

(documentation "https://github.com/winterchill-trading/parquet#readme")

(package
 (name parquet)
 (synopsis "Apache Parquet writer and reader in OCaml, for OxCaml")
 (description
  "Writes and reads Apache Parquet files with no Arrow or C++ dependency. The Thrift compact protocol, the PLAIN, dictionary, RLE/bit-packed and DELTA encodings, definition levels for nullable columns, statistics and the file footer are implemented in OCaml; zstd compression goes through libzstd, and the reader also decompresses snappy and lz4_raw. Columns are described with typed combinators over a row type, rows are buffered column by column with statistics, and a direct per-column API appends values without allocating. Requires the OxCaml compiler (unboxed floats) and the Jane Street libraries from its opam repository.")
 (tags
  (parquet columnar storage oxcaml))
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  (core
   (>= v0.18~preview))
  (ppx_jane
   (>= v0.18~preview))
  (zstd
   (>= 0.4))
  (core_unix
   (and
    :with-test
    (>= v0.18~preview)))))
