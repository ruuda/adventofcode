(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "example.txt"

let () =
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  (* There is only a single line, but we can cut off the trailing newline. *)
  let input_line = List.hd in_lines in
  let parts = String.split_on_char ',' input_line in
  List.iter (Printf.printf "%s\n") parts;
  flush stdout
