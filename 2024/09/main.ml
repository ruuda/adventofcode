(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "input.txt"

type file = {
  id : int;
  off : int;
  len : int;
};;

(* Parse the dense disk map, gaps are represented as file id -1. *)
let rec parse_disk : int -> int -> string -> file list = fun fid off disk_map ->
  let len = String.length disk_map in
  if len >= 2 then
    let file_len = int_of_string (String.sub disk_map 0 1) in
    let gap_len = int_of_string (String.sub disk_map 1 1) in
    let file_part = { id = fid; off = off; len = file_len } in
    let gap_part = { id = -1; off = off + file_len; len = gap_len } in
    let tail = parse_disk
      (fid + 1)
      (off + file_len + gap_len)
      (String.sub disk_map 2 (len - 2)) in
    file_part :: gap_part :: tail
  else if len == 1 then
    let file_len = int_of_string disk_map in
    let file_part = { id = fid; off = off; len = file_len } in
    [file_part]
  else
    []

(* Expand the dense disk map into a list with one element per sector.
   File id -1 represents a gap. *)
let rec expand_disk_map : file list -> int list = fun files ->
  match files with
    | [] -> []
    | f :: more ->
        let sectors = (List.init f.len (fun _i -> f.id)) in
        List.append sectors (expand_disk_map more)

let rec defragment1 : int list -> int list = fun sectors ->
  if List.length sectors < 2 then
    sectors
  else
    (* Very inefficient, I know, but it will do. *)
    let h = List.hd sectors in
    let rev_sectors = List.rev sectors in
    let t = List.hd rev_sectors in
    if t == -1 then
      (* If there is a gap at the end, just drop it. *)
      defragment1 (List.rev (List.tl rev_sectors))
    else if h == -1 then
      (* If we have a gap, move the last sector in there. *)
      let tail = List.rev (List.tl (List.rev (List.tl sectors))) in
      List.cons t (defragment1 tail)
    else
      (* If we have no gap, leave in place, defragment the remainder. *)
      List.cons h (defragment1 (List.tl sectors))

(* Place file f in the first gap that fits it. f must not be in the list. *)
let rec place : file list -> file -> file list = fun files f ->
  match files with
    | [] -> []
    | h :: t ->
      if h.len >= f.len && h.id == -1 then
        let newf = { off = h.off; len = f.len; id = f.id } in
        let newg = { off = h.off + f.len; len = h.len - f.len; id = -1 } in
        if newg.len > 0 then
          newf :: newg :: t
        else
          newf :: t
      else
        h :: place t f

(* Punch a hole where the file with id fid used to be. *)
let rec punch : file list -> int -> file list = fun files fid ->
  match files with
    | [] -> []
    | h :: t ->
      if h.id == fid then
        { off = h.off; len = h.len; id = -1 } :: t
      else
        h :: punch t fid

let defragment2 : file list -> file list = fun files ->
  (* Walk all files backwards and try to place them one by one. *)
  List.fold_right (fun f fs -> place (punch fs f.id) f) files files

let checksum : int list -> int = fun sectors ->
  List.fold_left (fun acc x -> acc + x) 0
  (List.mapi (fun i fid -> if fid >= 0 then i * fid else 0) sectors)

let part = 2

let () =
  (* There is only a single line, but we can cut off the trailing newline. *)
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  let input = List.hd in_lines in
  let disk = parse_disk 0 0 input in
  List.iter (fun f -> Printf.printf "%i+%i->%i " f.off f.len f.id) disk;
  let defrag_disk =
    if part == 1 then
      defragment1 (expand_disk_map disk)
    else
      expand_disk_map (defragment2 disk)
  in
  Printf.printf "\n";
  List.iter (fun i -> Printf.printf "%i " i) defrag_disk;
  let ck = checksum defrag_disk in
  Printf.printf "\nChecksum: %i\n" ck;
  flush stdout
