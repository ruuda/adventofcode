#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  # Inputs are pairs of pairs, separated by , at the top level.
  FS = ","

  num_contains = 0
  num_overlaps = 0
}

function contains(outer, inner) {
  return outer[1] <= inner[1] && outer[2] >= inner[2];
}

function overlaps(outer, inner) {
  return (\
    (inner[1] >= outer[1] && inner[1] <= outer[2]) ||
    (inner[2] >= outer[1] && inner[2] <= outer[2]) \
  );
}

{
  split($1, sections1, "-")
  split($2, sections2, "-")
  num_contains += contains(sections1, sections2) || contains(sections2, sections1)
  num_overlaps += overlaps(sections1, sections2) || overlaps(sections2, sections1)
}

END {
  print("Part 1 answer:", num_contains)
  print("Part 2 answer:", num_overlaps)
}
