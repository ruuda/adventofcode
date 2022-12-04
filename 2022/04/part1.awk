#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  # Inputs are pairs of pairs, separated by , at the top level.
  FS = ","

  num_overlap = 0
}

function contains(outer, inner) {
  return outer[1] <= inner[1] && outer[2] >= inner[2];
}

{
  split($1, sections1, "-")
  split($2, sections2, "-")
  has_overlap = contains(sections1, sections2) || contains(sections2, sections1)
  num_overlap += has_overlap
}

END {
  print(num_overlap)
}
