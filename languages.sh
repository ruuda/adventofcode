#!/usr/bin/sh

# This script prints languages used in this repository, ordered from most lines
# of code to least lines of code. It also prints whether I am allowed to use
# that language for my next solution, according to my personal rule of only
# using new languages or languages that are in the bottom 50%.

tokei \
  --exclude '{*.txt,*.md,Makefile,*.edn,*.csproj,*.fsproj,languages.sh,flake.nix}' \
  --compact \
  --rsort code \
  . \
  | tac \
  | awk '
  $1 == "Total" {
    total = $4
    cumulative = 0
    print("Next,Language,%Used,%Cumulative,Files,Lines")
  }
  $4 ~ /[0-9]+/ && $1 != "Total" {
    fraction = $4 / total
    cumfraction = (cumulative + $4) / total
    decision = cumulative / total <= 0.6666 ? "[ ]" : "[x]"
    color = cumulative / total <= 0.6666 ? "31" : "32"
    if ($1 == "Headache") $1 = "Hare";
    printf("\x1b[%sm%s,%s,%5.1f,%5.1f,%d,%d\x1b[0m\n",
      color,
      decision,
      $1,
      fraction * 100.0,
      cumfraction * 100.0,
      $2,
      $4)
    cumulative += $4
  }
  ' \
  | column \
  --table \
  --table-right 3,4,5,6 \
  --output-separator '   ' \
  --separator ,
