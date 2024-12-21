// Execute by running `dotnet run` in this directory. Optionally pass
// `--configuration Release`.
module Main

open System

let input = System.IO.File.ReadAllText("example.txt").Trim()

let rec blink : String list -> String list = fun stones ->
  match stones with
    | "0" :: tail -> "1" :: blink tail
    | n :: tail ->
      let nl = n.Length
      if (nl % 2) = 0 then
        n.Substring(0, nl / 2) :: n.Substring(nl / 2, nl / 2) :: blink tail
      else
        (System.Int64.Parse(n) * 2024L).ToString() :: blink tail
    | [] -> []

[<TailCall>]
let rec blinkn : int -> String list -> String list = fun n stones ->
  match n with
    | 0 -> stones
    | _ -> blinkn (n - 1) (blink stones)

[<EntryPoint>]
let main args =
  let result = blinkn 2 (input.Split(" ") |> Array.toList)
  printfn "%s" ((List.length result).ToString())
  0
