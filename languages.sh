#!/usr/bin/sh

# This script prints languages used in this repository, ordered from most lines
# of code to least lines of code. It also prints whether I am allowed to use
# that language for my next solution, according to my personal rule of only
# using new languages or languages that are in the bottom 50%.

~/.cargo/bin/tokei \
  --exclude '{*.txt,*.md,Makefile}' \
  --compact \
  --rsort code \
  . \
  | tac \
  | awk '
  $1 == "Total" {
    total = $4
    cumulative = 0
    print("Language,%Used,%Cumulative,Files,Allowed Next")
  }
  $4 ~ /[0-9]+/ && $1 != "Total" {
    fraction = $4 / total
    cumfraction = (cumulative + $4) / total
    decision = cumulative / total <= 0.5 ? "No" : "Yes"
    color = cumulative / total <= 0.5 ? "31" : "32"
    printf("\x1b[%sm%s,%5.1f,%5.1f,%d,%s\x1b[0m\n",
      color,
      $1,
      fraction * 100.0,
      cumfraction * 100.0,
      $2,
      decision)
    cumulative += $4
  }
  ' \
  | column \
  --table \
  --table-right 2,3,4 \
  --output-separator '   ' \
  --separator ,
