# Run with "nix eval --file main.nix"

let
  fileContents = builtins.readFile ./input.txt;

  # Split in Nix is weird, it also returns matches of the regex to split on.
  # But we can filter those out, because they are lists, not strings.
  lines = builtins.filter
    (x: builtins.isString x && x != "")
    (builtins.split "\n" fileContents);

  opponentMove = x: builtins.substring 0 1 x;
  selfMove = x: builtins.substring 2 1 x;

  asNumber = x: {
    "A" = 0;
    "B" = 1;
    "C" = 2;
    "X" = 0;
    "Y" = 1;
    "Z" = 2;
  }.${x};

  # Bring an integer x into the range [-1, 1].
  mod3 = x:
    if x > 1
    then mod3 (x - 3)
    else (if x < -1 then mod3 (x + 3) else x);

  # Determine the outcome of a vs. b:
  # 1: a wins, 0: draw, -1: b wins.
  outcome = a: b: mod3 (asNumber a - asNumber b);

  # The value of a move that we play (1 for rock/x, 3 for scissors/z).
  shapeScore = moves: 1 + asNumber (selfMove moves);
  outcomeScore = moves: 3 - 3 * (outcome (opponentMove moves) (selfMove moves));
  score = move: (shapeScore move) + (outcomeScore move);

  sum = builtins.foldl' builtins.add 0;
in
  sum (builtins.map score lines)
