// Execute by running `dotnet run` in this directory. Optionally pass
// `--configuration Release`, but it doesn't make an observable difference
// to me from eyeballing the speed. Alternatively, run `dotnet fsi Main.fs`,
// but replace the `main` entry point with just a call to `solve` then.
module Main

open System

let readLines filePath = System.IO.File.ReadLines(filePath)

type Move = { direction: Char; distance: Int32; color: String }
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

let walkOutline: Move list -> Coord list = fun moves ->
  let rec f = fun movesLeft at ->
    match movesLeft with
      | { direction = dir; distance = 0; color = _ } :: tail -> f tail at
      | { direction = d; distance = n; color = c } :: tail ->
          let pos = moveDir d at;
          pos :: f ({direction = d; distance = (n - 1); color = c} :: tail) pos
      | [] -> []
  f moves (Coord(0, 0))

let rec floodFill: Coord Set -> Coord Set -> Coord Set =
  fun opens closed ->
    if Set.isEmpty opens then closed else
      let coord = Set.minElement opens;
      let closed' = Set.add coord closed;
      let isNotClosed = fun c -> not (Set.contains c closed);
      // Note, previously I expressed this with set union and difference, which
      // is maybe a bit more elegant and obvious, but which was also slow. Now
      // we do the minimal number of single-element modifications, and that is
      // *much* faster.
      let opens' =
        neighbors coord
        |> Seq.filter isNotClosed
        |> Seq.fold (fun acc c -> Set.add c acc) (Set.remove coord opens);
      if (Set.count opens') <> (Set.count opens) then
        printfn "Recurse, opens: %i, closed: %i" (Set.count opens') (Set.count closed');
      floodFill opens' closed'

let floodFill2 : Coord Set -> Coord Set -> uint64 =
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

let solve =
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
      Coord(topLeft.x + 1, topLeft.y)
      Coord(topLeft.x + 1, topLeft.y + 1)
      Coord(topLeft.x, topLeft.y + 1)
    ];
  let start = Set.minElement (Set.difference startCandidates outlineSet);
  printfn "Start cell is %i, %i" start.x start.y;

  let fill2 = floodFill2 outlineSet (Set.singleton start);
  let fill = floodFill (Set.singleton start) outlineSet;
  // for move in moves do
  //   printfn "Move %c %i %s" move.direction move.distance move.color
  // for coord in outline do
  //   printfn "Outline (%i %i)" coord.x coord.y
  printfn "Fill covers %i squares (1) %i (2)" (Set.count fill) fill2

[<EntryPoint>]
let main args =
  solve
  0
