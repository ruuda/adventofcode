// Execute by running `dotnet run` in this directory. Optionally pass
// `--configuration Release`, but it doesn't make an observable difference
// to me from eyeballing the speed. Alternatively, run `dotnet fsi Main.fs`,
// but replace the `main` entry point with just a call to `solve` then.
module Main

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

type Move = { direction: Char; distance: Int32; color: String }

// Take the direction and distance from the color, as required for part 2 of the
// puzzle.
let flip : Move -> Move =
  fun move ->
    let direction =
      match move.color[7] with
        | '0' -> 'R'
        | '1' -> 'D'
        | '2' -> 'L'
        | '3' -> 'U'
    let distance = Convert.ToInt32(move.color[2..7], 16);
    { direction = direction; distance = distance; color = move.color }

type Coord =
  struct
    val x: int
    val y: int
    new (x, y) = { x = x; y = y }
  end

let readFile: String -> Move seq = fun fname ->
  Seq.map
    (fun (line: String) ->
      let [dir; dist; color] = line.Split(" ") |> List.ofArray;
      { Move.direction = dir.[0]; distance = int dist; color = color }
    )
    (readLines fname)

let moveDir: Char -> Coord -> Coord = fun dir coord ->
  match dir with
    | 'U' -> Coord(coord.x, coord.y - 1)
    | 'D' -> Coord(coord.x, coord.y + 1)
    | 'L' -> Coord(coord.x - 1, coord.y)
    | 'R' -> Coord(coord.x + 1, coord.y)

let neighbors: Coord -> Coord list =
  fun coord ->
    [ Coord(coord.x + 1, coord.y)
      Coord(coord.x - 1, coord.y)
      Coord(coord.x, coord.y + 1)
      Coord(coord.x, coord.y - 1)
    ]

let walkOutline: Move list -> Coord Set = fun moves ->
  let rec f = fun movesLeft at res ->
    match movesLeft with
      | { direction = dir; distance = 0; color = _ } :: tail ->
          printfn "Frontier length: %i" (Set.count res);
          f tail at res
      | { direction = d; distance = n; color = c } :: tail ->
          let pos = moveDir d at;
          f
            ({direction = d; distance = (n - 1); color = c} :: tail)
            pos
            (Set.add pos res)
      | [] -> res
  f moves (Coord(0, 0)) Set.empty

let floodFill : Coord Set -> Coord Set -> uint64 =
  fun boundary frontier ->
    let rec step : Coord Set -> Coord Set -> uint64 -> uint64 =
      fun prevFrontier currFrontier count ->
        if Set.isEmpty currFrontier then count else
          // Compute the entire new frontier in one go. The frontier consists of all
          // neighbors of the current frontier, that do not lie on the boundary, and
          // that do not lie on the current or previous frontier. Any frontiers
          // further back we don't need to care about, because they are entirely
          // enclosed by the current frontier.
          let nextFrontier =
            seq {
              for c0 in currFrontier do
                for coord in neighbors c0 do
                  if not (Set.contains coord boundary) then
                    if not (Set.contains coord currFrontier) then
                      if not (Set.contains coord prevFrontier) then
                        coord
            }
          let currCount = uint64 (Set.count currFrontier);
          printfn "Step: frontier %i, count: %i" (Set.count currFrontier) count;
          step currFrontier (Set.ofSeq nextFrontier) (count + currCount)

    // Initiate the recursion with an empty previous frontier, whatever frontier
    // the caller seeds us with, and the count is 0 + the size of the boundary,
    // because the boundary is included.
    step Set.empty frontier (uint64 (Set.count boundary))

// Figure out a good place to start our flood fill, by finding the leftmost,
// and as tiebreaker, topmost, square. Then take a neighbord that is below or
// to the right of it, but not on the boundary.
let startCoord : Coord Set -> Coord =
  fun outline ->
    let topLeft = Set.minElement outline;
    let candidates =
      [
        Coord(topLeft.x + 1, topLeft.y)
        Coord(topLeft.x + 1, topLeft.y + 1)
        Coord(topLeft.x, topLeft.y + 1)
      ];
    let candidates' =
      seq {
        for coord in candidates do
          if not (Set.contains coord outline) then
            coord
      };
    Set.minElement (Set.ofSeq candidates')

let solve =
  let moves = readFile "example.txt"

  let outline = walkOutline (List.ofSeq moves);
  printfn "Part 1: The outline covers %i squares" (Set.count outline);
  let fill = floodFill outline (Set.singleton (startCoord outline));
  printfn "Part 1: Fill covers %i squares." fill

  // TODO: Probably the way to do part 2 is to make some kind of grid data
  // structure, where a horizontal or vertical move for drawing the outline
  // doesn't touch O(distance) cells, we merely note the width of that area, and
  // if the area ends in the middle of some region, we cut up the region. Then
  // we do the flood fill on those larger cells. So we need in the worst case
  // O(lines²) memory instead of O(sum(line-lengths)). But I don't care to do
  // that right now.
  let moves2 = Seq.map flip moves;
  let outline2 = walkOutline (List.ofSeq moves2);
  printfn "Part 2: The outline covers %i squares" (Set.count outline2);
  let fill2 = floodFill outline2 (Set.singleton (startCoord outline2));
  printfn "Part 2: Fill covers %i squares." fill2

[<EntryPoint>]
let main args =
  solve
  0
