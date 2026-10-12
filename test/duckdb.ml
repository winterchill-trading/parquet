open! Core

let with_parquet_file f =
  let dir = Filename_unix.temp_dir "parquet" "test" in
  Exn.protect
    ~f:(fun () -> f (dir ^/ "test.parquet"))
    ~finally:(fun () ->
      Array.iter (Sys_unix.readdir dir) ~f:(fun name -> Sys_unix.remove (dir ^/ name));
      Core_unix.rmdir dir)
;;

let query ~file sql =
  let sql = String.substr_replace_all sql ~pattern:"$file" ~with_:(sprintf "'%s'" file) in
  let command = sprintf "duckdb -csv -c %s 2>&1" (Filename.quote sql) in
  let ic = Core_unix.open_process_in command in
  let output = In_channel.input_all ic in
  (match Core_unix.close_process_in ic with
   | Ok () -> ()
   | Error _ -> print_endline "(duckdb exited with an error)");
  print_string output
;;
