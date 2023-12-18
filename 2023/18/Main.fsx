#!/usr/bin/env -S dotnet fsi

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

type Move = { direction: Char; distance: Int32; color: String }
type Coord = { x: Int32; y: Int32 }

let readFile: String -> Move seq = fun fname ->
  Seq.map
    (fun (line: String) ->
      let [dir; dist; color] = line.Split(" ") |> List.ofArray;
      { Move.direction = dir[0]; distance = int dist; color = color }
    )
    (readLines fname)

let moveDir: Char -> Coord -> Coord = fun dir coord ->
  match dir with
    | 'U' -> { x = coord.x; y = coord.y - 1 }
    | 'D' -> { x = coord.x; y = coord.y + 1 }
    | 'L' -> { x = coord.x - 1; y = coord.y }
    | 'R' -> { x = coord.x + 1; y = coord.y }

let neighbors: Coord -> Coord list = fun coord -> [
  { x = coord.x + 1; y = coord.y }
  { x = coord.x - 1; y = coord.y }
  { x = coord.x; y = coord.y + 1 }
  { x = coord.x; y = coord.y - 1 }
]

let walkOutline: Move list -> Coord list = fun moves ->
  let rec f = fun movesLeft at ->
    match movesLeft with
      | { direction = dir; distance = 0; color = _ } :: tail -> f tail at
      | { direction = d; distance = n; color = c } :: tail ->
          let pos = moveDir d at;
          pos :: f ({direction = d; distance = (n - 1); color = c} :: tail) pos
      | [] -> []
  f moves { x = 0; y = 0 }

let rec floodFill: Coord Set -> Coord Set -> Coord Set =
  fun opens closed ->
    if Set.isEmpty opens then closed else
      let coord = Set.minElement opens;
      let closed' = Set.add coord closed;
      let newOpen = Set.ofList (neighbors coord);
      let opens' = Set.union (Set.remove coord opens) (Set.difference newOpen closed);
      if (Set.count opens') <> (Set.count opens) then
        printfn "Recurse, opens: %i, closed: %i" (Set.count opens') (Set.count closed');
      floodFill opens' closed'

let main =
  let moves = readFile "input.txt"
  let outlineSeq = walkOutline (List.ofSeq moves);
  let lastCell = List.head outlineSeq;
  let outlineSet = Set.ofSeq outlineSeq;
  printfn "The outline covers %i squares" (Set.count outlineSet);

  // Figure out a good place to start our flood fill, by finding the leftmost,
  // and as tiebreaker, topmost, square. Then take a neighbord that is below or
  // to the right of it, but not on the boundary.
  let topLeft = Set.minElement outlineSet;
  let startCandidates =
    Set.ofSeq [
      { x = topLeft.x + 1; y = topLeft.y }
      { x = topLeft.x + 1; y = topLeft.y + 1 }
      { x = topLeft.x; y = topLeft.y + 1 }
    ];
  let start = Set.minElement (Set.difference startCandidates outlineSet);
  printfn "Start cell is %i, %i" start.x start.y;

  let fill = floodFill (Set.singleton start) outlineSet;
  // for move in moves do
  //   printfn "Move %c %i %s" move.direction move.distance move.color
  // for coord in outline do
  //   printfn "Outline (%i %i)" coord.x coord.y
  printfn "Fill covers %i squares" (Set.count fill)

main 
