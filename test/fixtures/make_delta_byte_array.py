# Writes delta_byte_array.parquet, the DELTA_BYTE_ARRAY fixture (pyarrow, data page v2,
# several pages per column):
#   uv run --no-project --with pyarrow python make_delta_byte_array.py delta_byte_array.parquet
import sys, pyarrow as pa, pyarrow.parquet as pq
out = sys.argv[1]
words = ["axis", "axle", "babble", "babyhood", None, "", "baby", "babyhood", "zebra", "zebras"] * 30
nums = [None if i % 7 == 0 else i * 1000003 - 5 for i in range(300)]
t = pa.table({"w": pa.array(words, pa.string()), "n": pa.array(nums, pa.int64())})
pq.write_table(t, out, use_dictionary=False, compression="none",
               column_encoding={"w": "DELTA_BYTE_ARRAY", "n": "DELTA_BINARY_PACKED"},
               data_page_version="2.0", data_page_size=512)
