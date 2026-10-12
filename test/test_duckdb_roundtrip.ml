open! Core
open Parquet

type row =
  { id : int
  ; big : int
  ; px : float
  ; coin : string
  ; raw : string
  ; ts : Time_ns.t
  ; notional : int
  ; is_buy : bool
  }

let schema =
  Schema.create
    Schema.Column.
      [ int32 "id" (fun r -> r.id)
      ; int64 "big" (fun r -> r.big)
      ; float "px" (fun r -> r.px)
      ; utf8 "coin" (fun r -> r.coin)
      ; bytes "raw" (fun r -> r.raw)
      ; timestamp_ns "ts" (fun r -> r.ts)
      ; decimal ~precision:18 ~scale:4 "notional" (fun r -> r.notional)
      ; bool "is_buy" (fun r -> r.is_buy)
      ]
;;

let row i =
  { id = i
  ; big = (i - 2) * 1_000_000_000_000
  ; px = (Float.of_int i *. 1.5) -. 3.
  ; coin = List.nth_exn [ "BTC"; "ETH"; "SOL"; "héllo" ] (i % 4)
  ; raw = String.make (i % 3) 'x'
  ; ts = Time_ns.of_int_ns_since_epoch (1_700_000_000_000_000_000 + i)
  ; notional = (i * 12_345) - 50_000
  ; is_buy = i % 3 = 0
  }
;;

let write ?row_group_rows ?compression file rows =
  Writer.with_file ~schema ?row_group_rows ?compression file ~f:(fun w ->
    Writer.add_all w rows)
;;

let%expect_test "logical types as seen by duckdb" =
  Duckdb.with_parquet_file (fun file ->
    write file (List.init 3 ~f:row);
    Duckdb.query
      ~file
      "select column_name, column_type from (describe select * from $file)");
  [%expect
    {|
    column_name,column_type
    id,INTEGER
    big,BIGINT
    px,DOUBLE
    coin,VARCHAR
    raw,BLOB
    ts,TIMESTAMP_NS
    notional,"DECIMAL(18,4)"
    is_buy,BOOLEAN
    |}]
;;

let%expect_test "values round trip" =
  Duckdb.with_parquet_file (fun file ->
    write file (List.init 10 ~f:row);
    Duckdb.query ~file "select *, epoch_ns(ts) as ns from $file order by id");
  [%expect
    {|
    id,big,px,coin,raw,ts,notional,is_buy,ns
    0,-2000000000000,-3.0,BTC,,2023-11-14 22:13:20,-5.0000,true,1700000000000000000
    1,-1000000000000,-1.5,ETH,x,2023-11-14 22:13:20.000000001,-3.7655,false,1700000000000000001
    2,0,0.0,SOL,xx,2023-11-14 22:13:20.000000002,-2.5310,false,1700000000000000002
    3,1000000000000,1.5,"héllo",,2023-11-14 22:13:20.000000003,-1.2965,true,1700000000000000003
    4,2000000000000,3.0,BTC,x,2023-11-14 22:13:20.000000004,-0.0620,false,1700000000000000004
    5,3000000000000,4.5,ETH,xx,2023-11-14 22:13:20.000000005,1.1725,false,1700000000000000005
    6,4000000000000,6.0,SOL,,2023-11-14 22:13:20.000000006,2.4070,true,1700000000000000006
    7,5000000000000,7.5,"héllo",x,2023-11-14 22:13:20.000000007,3.6415,false,1700000000000000007
    8,6000000000000,9.0,BTC,xx,2023-11-14 22:13:20.000000008,4.8760,false,1700000000000000008
    9,7000000000000,10.5,ETH,,2023-11-14 22:13:20.000000009,6.1105,true,1700000000000000009
    |}]
;;

let%expect_test "row groups carry min/max statistics" =
  Duckdb.with_parquet_file (fun file ->
    write file ~row_group_rows:4 (List.init 10 ~f:row);
    Duckdb.query
      ~file
      {|select row_group_id, row_group_num_rows, path_in_schema, stats_min_value, stats_max_value
        from parquet_metadata($file)
        where path_in_schema in ('id', 'px', 'coin', 'notional')
        order by row_group_id, column_id|});
  [%expect
    {|
    row_group_id,row_group_num_rows,path_in_schema,stats_min_value,stats_max_value
    0,4,id,0,3
    0,4,px,-3.0,1.5
    0,4,coin,BTC,"héllo"
    0,4,notional,-5.0000,-1.2965
    1,4,id,4,7
    1,4,px,3.0,7.5
    1,4,coin,BTC,"héllo"
    1,4,notional,-0.0620,3.6415
    2,2,id,8,9
    2,2,px,9.0,10.5
    2,2,coin,BTC,ETH
    2,2,notional,4.8760,6.1105
    |}]
;;

let%expect_test "zstd compressed pages" =
  Duckdb.with_parquet_file (fun file ->
    write file ~compression:`Zstd ~row_group_rows:3 (List.init 10 ~f:row);
    Duckdb.query ~file "select distinct compression from parquet_metadata($file)";
    Duckdb.query
      ~file
      "select count(*), sum(id), sum(notional), string_agg(coin, '' order by id) from \
       $file");
  [%expect
    {|
    compression
    ZSTD
    count_star(),sum(id),sum(notional),"string_agg(coin, '' ORDER BY id)"
    10,45,5.5525,"BTCETHSOLhélloBTCETHSOLhélloBTCETH"
    |}]
;;

let%expect_test "a file without rows" =
  Duckdb.with_parquet_file (fun file ->
    write file [];
    Duckdb.query ~file "select count(*) from $file";
    Duckdb.query ~file "select num_rows, num_row_groups from parquet_file_metadata($file)");
  [%expect {|
    count_star()
    0
    num_rows,num_row_groups
    0,0
    |}]
;;
