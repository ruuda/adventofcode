(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "example.txt"

let () =
  let result = In_channel.with_open_text "example.txt" In_channel.input_lines in
  let _ = List.map print_endline result in
  flush stdout;
