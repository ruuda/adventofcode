// Execute by running `dotnet run` in this directory. Optionally pass
// `--configuration Release`.
module Main

open System

let input = System.IO.File.ReadAllText("input.txt").Trim()

let cut : String -> String = fun s ->
  match s.TrimStart('0') with
    | "" -> "0"
    | st -> st

// Return the number of stones after n blinks, when starting with `stone`.
let rec blinkn : int -> String -> int = fun n stone ->
  match n with
    | 0 ->
      // printfn "%s " stone
      1
    | _ ->
      match stone with
        | "0" -> blinkn (n - 1) "1"
        | _ ->
          let k = stone.Length
          if (k % 2) = 0 then
            (blinkn (n - 1) (stone.Substring(0, k / 2))) +
              (blinkn (n - 1) (cut (stone.Substring(k / 2, k / 2))))
          else
            blinkn (n - 1) ((System.Int64.Parse(stone) * 2024L).ToString())
       

[<EntryPoint>]
let main args =
  let result = input.Split(" ") |> Array.map (blinkn 25) |> Array.sum
  printfn "%i" result
  0
