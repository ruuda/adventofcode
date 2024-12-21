// Execute by running `dotnet run` in this directory. Optionally pass
// `--configuration Release`.
module Main

open System

let input = System.IO.File.ReadAllText("example.txt")

[<EntryPoint>]
let main args =
  printfn "%s" input
  0
