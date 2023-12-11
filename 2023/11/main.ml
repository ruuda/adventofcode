(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "example.txt"

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

let () =
  let map = In_channel.with_open_text "example.txt" In_channel.input_lines in
  List.iter print_endline map;
  let galaxies = locate_galaxies map in
  List.iter (fun (x, y) -> Printf.printf "%i %i\n" x y) galaxies;
  flush stdout
