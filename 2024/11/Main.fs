// Execute by running `dotnet run` in this directory. Optionally pass
// `--configuration Release`.
module Main

open System
open System.Collections.Generic

let input = System.IO.File.ReadAllText("input.txt").Trim()

let cut : String -> String = fun s ->
  match s.TrimStart('0') with
    | "" -> "0"
    | st -> st

// Return the number of stones after n blinks, when starting with `stone`.
let rec blinkn : Dictionary<string, int> array -> int -> String -> int = fun cache n stone ->
  match n with
    | 0 -> 1
    | _ ->
      if cache[n].ContainsKey(stone) then
        cache[n].Item(stone)
      else
        let result =
          match stone with
            | "0" -> blinkn cache (n - 1) "1"
            | _ ->
              let k = stone.Length
              if (k % 2) = 0 then
                (blinkn cache (n - 1) (stone.Substring(0, k / 2))) +
                  (blinkn cache (n - 1) (cut (stone.Substring(k / 2, k / 2))))
              else
                blinkn cache (n - 1) ((System.UInt64.Parse(stone) * 2024UL).ToString())
        cache[n].Add(stone, result)
        result

[<EntryPoint>]
let main args =
  let cache = Array.init 76 (fun _ -> new Dictionary<string, int>())
  let result = input.Split(" ") |> Array.map (blinkn cache 75) |> Array.sum
  printfn "%i" result
  0