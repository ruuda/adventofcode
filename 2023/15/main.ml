(* Run with `ocaml main.ml` or `ocamlopt -o main main.ml && main`. *)
let file = "input.txt"

let hash : string -> int = fun chars ->
  String.fold_left (fun acc ch -> ((acc + (Char.code ch)) * 17) mod 256) 0 chars

type instr = Remove | Assign of int
let parse_instr : string -> string * instr = fun part ->
  match String.index_opt part '=' with
    | Some i -> (
      String.sub part 0 i,
      Assign (int_of_string (String.sub part (i + 1) ((String.length part) - i - 1)))
    )
    (* It has to be a Remove ending in "-", cut that off. *)
    | None -> String.sub part 0 ((String.length part) - 1), Remove

(* Why is the OCaml stdlib so bare I have to define this myself? *)
let apply : 'a array -> int -> ('a -> 'a) -> unit = fun arr i f ->
  let x = Array.get arr i in Array.set arr i (f x)

(* Implementation of the two instructions from part 2. *)
let rec do_remove : string -> (string * int) list -> (string * int) list =
  fun label xs -> match xs with
    | [] -> []
    | (ll, _) :: tail when String.equal ll label -> tail
    | (ll, n) :: tail -> (ll, n) :: do_remove label tail

let rec do_assign : string -> int -> (string * int) list -> (string * int) list =
  fun label n xs -> match xs with
    | [] -> [(label, n)]
    | (ll, _) :: tail when String.equal ll label -> (label, n) :: tail
    | (ll, m) :: tail -> (ll, m) :: do_assign label n tail

let focus_power : (string * int) list -> int = fun xs ->
  List.fold_left (+) 0 (List.mapi (fun i (_, n) -> (i + 1) * n) xs)

let print_boxes : (string * int) list array -> unit =
  Array.iteri (fun i xs ->
    if List.length xs > 0 then (
      Printf.printf "Box %i:" i;
      List.iter (fun (ll, n) -> Printf.printf " [%s %i]" ll n) xs;
      Printf.printf "\n"
    )
  )

(* Execute the HASHMAP algorithm from part 2. Returns the focusing power per box. *)
let hashmap : string list -> int array = fun parts ->
  let boxes = Array.make 256 [] in
  let instrs = List.map parse_instr parts in
  let handle_instr = fun (label, instr) ->
    let i = hash label in
    (match instr with
      | Remove -> Printf.printf "box %i rm %s\n" i label; apply boxes i (do_remove label)
      | Assign k -> Printf.printf "box %i assign %s = %i\n" i label k; apply boxes i (do_assign label k)
    );
    print_boxes boxes
  in
  List.iter handle_instr instrs;
  Array.mapi (fun i xs -> (i + 1) * (focus_power xs)) boxes

let () =
  let in_lines = In_channel.with_open_text file In_channel.input_lines in
  (* There is only a single line, but we can cut off the trailing newline. *)
  let input_line = List.hd in_lines in
  let parts = String.split_on_char ',' input_line in
  let part1 = List.fold_left (+) 0 (List.map hash parts) in
  let part2 = Array.fold_left (+) 0 (hashmap parts) in
  (*Array.iter (Printf.printf "%i\n") (hashmap parts); *)
  Printf.printf "Part 1: %i\nPart 2: %i\n" part1 part2;
  flush stdout
