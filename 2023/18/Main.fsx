#!/usr/bin/env -S dotnet fsi

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

type Move = { direction: Char; distance: Int32; color: String }

let readFile: String -> Move seq = fun fname ->
  Seq.map
    (fun (line: String) ->
      let [dir; dist; color] = line.Split(" ") |> List.ofArray;
      { Move.direction = dir[0]; distance = int dist; color = color }
    )
    (readLines fname)

let main =
  let moves = readFile "example.txt"
  for move in moves do
    printfn "Move %c %i %s" move.direction move.distance move.color

main 
