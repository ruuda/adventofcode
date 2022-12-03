# Run with "nix eval --file main.nix"

let
  fileContents = builtins.readFile ./input.txt;

  # Split in Nix is weird, it also returns matches of the regex to split on.
  # But we can filter those out, because they are lists, not strings.
  lines = builtins.filter
    (x: builtins.isString x && x != "")
    (builtins.split "\n" fileContents);

  rucksack = contents:
    let
      n = builtins.stringLength contents;
    in
      {
        lhs = builtins.substring 0 (n / 2) contents;
        rhs = builtins.substring (n / 2) (n / 2) contents;
      };

  foldlString = f: z: str:
    let
      n = builtins.stringLength str;
      head = builtins.substring 0 1 str;
      tail = builtins.substring 1 (n - 1) str;
    in
      if n == 0
      then z
      else f head (foldlString f z tail);

  contains = needle: haystack:
  let
    check = q: z: if q == needle then true else z;
  in
    foldlString check false haystack;

  findDup = { lhs, rhs }:
    let
      appendDup = q: z: if contains q rhs then q else z;
    in
      foldlString appendDup "" lhs;

  # Return the priority of an item, which is its index in this string (1-based).
  priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  priority = q:
    let
      go = n:
        if q == builtins.substring n 1 priorities
        then n + 1
        else go (n + 1);
    in
      go 0;
in
  builtins.foldl'
    builtins.add
    0
    (builtins.map (x: priority (findDup (rucksack x))) lines)
