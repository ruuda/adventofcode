(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "example.txt"

(* Parse the dense disk map into a list with one element per sector.
   File id -1 represents a gap. *)
let rec parse_disk : int -> string -> int list = fun fid disk_map ->
  let len = String.length disk_map in
  if len >= 2 then
    let file_len = int_of_string (String.sub disk_map 0 1) in
    let gap_len = int_of_string (String.sub disk_map 1 1) in
    let file_part = List.init file_len (fun _i -> fid) in
    let gap_part = List.init gap_len (fun _i -> -1) in
    let tail = parse_disk (fid + 1) (String.sub disk_map 2 (len - 2)) in
    List.append file_part (List.append gap_part tail)
  else if len == 1 then
    let file_len = int_of_string disk_map in
    List.init file_len (fun _i -> fid)
  else
    []

let () =
  (* There is only a single line, but we can cut off the trailing newline. *)
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  let input = List.hd in_lines in
  let disk = parse_disk 0 input in
  List.iter (fun i -> Printf.printf "%i " i) disk;
  flush stdout
