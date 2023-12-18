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

let walkOutline: Move list -> Coord list = fun moves ->
  let rec f = fun movesLeft at ->
    match movesLeft with
      | { direction = dir; distance = 0; color = _ } :: tail -> f tail at
      | { direction = d; distance = n; color = c } :: tail ->
          let pos = moveDir d at;
          pos :: f ({direction = d; distance = (n - 1); color = c} :: tail) pos
      | [] -> []
  f moves { x = 0; y = 0 }

let main =
  let moves = readFile "example.txt"
  let outline = walkOutline (List.ofSeq moves);
  for move in moves do
    printfn "Move %c %i %s" move.direction move.distance move.color
  for coord in outline do
    printfn "Outline (%i %i)" coord.x coord.y

main 
