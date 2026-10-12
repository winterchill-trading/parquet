open! Core

let print s =
  String.to_list s
  |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  |> String.concat ~sep:" "
  |> print_endline
;;
