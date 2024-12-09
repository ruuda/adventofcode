(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "input.txt"

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

let rec defragment : int list -> int list = fun sectors ->
  if List.length sectors < 2 then
    sectors
  else
    (* Very inefficient, I know, but it will do. *)
    let h = List.hd sectors in
    let rev_sectors = List.rev sectors in
    let t = List.hd rev_sectors in
    if t == -1 then
      (* If there is a gap at the end, just drop it. *)
      defragment (List.rev (List.tl rev_sectors))
    else if h == -1 then
      (* If we have a gap, move the last sector in there. *)
      let tail = List.rev (List.tl (List.rev (List.tl sectors))) in
      List.cons t (defragment tail)
    else
      (* If we have no dap, leave in place, defragment the remainder. *)
      List.cons h (defragment (List.tl sectors))

let checksum : int list -> int = fun sectors ->
  List.fold_left (fun acc x -> acc + x) 0
  (List.mapi (fun i fid -> i * fid) sectors)

let () =
  (* There is only a single line, but we can cut off the trailing newline. *)
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  let input = List.hd in_lines in
  let disk = parse_disk 0 input in
  List.iter (fun i -> Printf.printf "%i " i) disk;
  let defrag_disk = defragment disk in
  Printf.printf "\n";
  List.iter (fun i -> Printf.printf "%i " i) defrag_disk;
  let ck = checksum defrag_disk in
  Printf.printf "\nChecksum: %i\n" ck;
  flush stdout
