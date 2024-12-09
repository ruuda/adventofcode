(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "example.txt"

let () =
  (* There is only a single line, but we can cut off the trailing newline. *)
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  let input = List.hd in_lines in

  Printf.printf "Input: %s\n" input;
  flush stdout
