(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "example.txt"

module IntSet = Set.Make(Int);;

let locate_galaxies: string list -> (int * int) list = fun rows ->
  let fold_row = fun y acc row ->
    String.fold_left
      (fun (x, res) ch -> match ch with
        | '.' -> (x + 1, res)
        | '#' -> (x + 1, (x, y) :: res)
        | _ -> failwith "Unexpected character on the map."
      ) (0, acc) row in
  let (_, res): int * (int * int) list = List.fold_left
    (fun (y, res) row -> let (_, r) = fold_row y res row in (y + 1, r))
    (0, []) rows in
  res

let locate_empty_rows: string list -> int list = fun rows ->
  let (_, res) = List.fold_left
    (fun (y, res) row ->
      if String.contains row '#'
      then (y + 1, res)
      else (y + 1, y :: res)
    )
    (0, []) rows in
  res

let locate_empty_columns: string list -> int list = fun rows ->
  let empties = fun row ->
    let (_, res) = String.fold_left
      (fun (x, res) ch -> match ch with
        | '.' -> (x + 1, IntSet.add x res)
        | '#' -> (x + 1, res)
        | _ -> failwith "Unexpected character on the map."
      ) (0, IntSet.empty) row in
    res in
  let sets = List.map empties rows in
  let res = List.fold_left IntSet.inter (List.hd sets) sets in
  List.rev (IntSet.elements res)

let () =
  let map = In_channel.with_open_text "example.txt" In_channel.input_lines in
  List.iter print_endline map;
  let galaxies = locate_galaxies map in
  List.iter (fun (x, y) -> Printf.printf "%i %i\n" x y) galaxies;
  List.iter (Printf.printf "COL %i\n") (locate_empty_columns map);
  List.iter (Printf.printf "ROW %i\n") (locate_empty_rows map);
  flush stdout
