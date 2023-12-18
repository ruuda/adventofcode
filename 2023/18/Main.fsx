#!/usr/bin/env -S dotnet fsi

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

let main =
  let lines = readLines "example.txt"
  for line in lines do
    printfn "Line is %s" line

main 
