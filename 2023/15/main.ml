(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "input.txt"

let hash : string -> int = fun chars ->
  String.fold_left (fun acc ch -> ((acc + (Char.code ch)) * 17) mod 256) 0 chars

let () =
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  (* There is only a single line, but we can cut off the trailing newline. *)
  let input_line = List.hd in_lines in
  let parts = String.split_on_char ',' input_line in
  let part1 = List.fold_left (+) 0 (List.map hash parts) in
  Printf.printf "Part 1: %i\n" part1;
  flush stdout
