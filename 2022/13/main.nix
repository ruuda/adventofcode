# Run with "nix eval --file main.nix"

let
  fileContents = builtins.readFile ./input.txt;

  # Split in Nix is weird, it also returns matches of the regex to split on.
  # But we can filter those out, because they are lists, not strings.
  split = needle: haystack: builtins.filter
    (x: builtins.isString x && x != "")
    (builtins.split needle haystack);

  rawPairs = split "\n\n" fileContents;

  parsePair = pair: builtins.map builtins.fromJSON (split "\n" pair);

  compareInt = lhs: rhs: cont:
    if lhs < rhs
    then true
    else if lhs > rhs
    then false
    else cont;

  compareList = lhs: rhs: cont:
    if lhs == [] && rhs == []
    then cont
    else if lhs == []
    then true
    else if rhs == []
    then false
    else
      compare
        (builtins.head lhs) (builtins.head rhs)
        (compare (builtins.tail lhs) (builtins.tail rhs) cont);

  compare = lhs: rhs: cont:
    if (builtins.isInt lhs) && (builtins.isInt rhs)
    then compareInt lhs rhs cont
    else if (builtins.isList lhs) && (builtins.isList rhs)
    then compareList lhs rhs cont
    else if builtins.isList lhs
    then compareList lhs [rhs] cont
    else if builtins.isList rhs
    then compareList [lhs] rhs cont
    else builtins.throw "Impossible";

  comparePair = pairStr:
    let
      pair = parsePair pairStr;
      lhs = builtins.elemAt pair 0;
      rhs = builtins.elemAt pair 1;
    in
      compare lhs rhs false;

  wellOrderedPairs = builtins.map comparePair rawPairs;

  sumTrueIndices = i: n: xs:
    if xs == []
    then n
    else sumTrueIndices (i + 1) (if builtins.head xs then n + i else n) (builtins.tail xs);

in
  sumTrueIndices 1 0 wellOrderedPairs
