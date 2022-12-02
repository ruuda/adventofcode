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
  outcome = a: b: mod3 (a - b);

  # The value of a move that we play (1 for rock/x, 3 for scissors/z).
  shapeScore = a: b: 1 + b;
  outcomeScore = a: b: 3 - 3 * (outcome a b);
  score = a: b: (shapeScore a b) + (outcomeScore a b);

  # Parse the line into numeric moves a and b, then call f(a, b).
  ap2 = f: moves: f (asNumber (opponentMove moves)) (asNumber (selfMove moves));

  sum = builtins.foldl' builtins.add 0;

  answerPartA = sum (builtins.map (ap2 score) lines);

  # Like mod3, but put the result in [0, 2] instead of [-1, 1].
  mod3' = x:
    if x > 2
    then mod3' (x - 3)
    else (if x < 0 then mod3' (x + 3) else x);

  # Get the move to play according to the instruction, which is 0 for losing,
  # 1 for a draw, and 2 for winning.
  getMove = a: instr: mod3' (a - 1 + instr);

  scoreB = a: instr: score a (getMove a instr);
  answerPartB = sum (builtins.map (ap2 scoreB) lines);
in
  answerPartB
