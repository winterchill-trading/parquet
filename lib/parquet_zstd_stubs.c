/* ZSTD_compress over OCaml bytes, without the copies the ctypes binding makes. The
   prototypes are declared here so no zstd header is needed; libzstd itself is linked
   through the zstd opam library. Neither call allocates on the OCaml heap, so the bytes
   cannot move while zstd reads and writes them. */

#include <stddef.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

extern size_t ZSTD_compress(void *dst, size_t dst_capacity, const void *src,
                            size_t src_size, int level);
extern size_t ZSTD_compressBound(size_t src_size);
extern unsigned ZSTD_isError(size_t code);
extern const char *ZSTD_getErrorName(size_t code);

value parquet_zstd_compress_bound(value v_len)
{
  return Val_long(ZSTD_compressBound((size_t)Long_val(v_len)));
}

value parquet_zstd_compress(value v_level, value v_src, value v_len, value v_dst)
{
  size_t r = ZSTD_compress(Bytes_val(v_dst), caml_string_length(v_dst),
                           Bytes_val(v_src), (size_t)Long_val(v_len),
                           Int_val(v_level));
  if (ZSTD_isError(r)) caml_failwith(ZSTD_getErrorName(r));
  return Val_long(r);
}
