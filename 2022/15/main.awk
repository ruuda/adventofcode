#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  FS = " "
  # Target row to investigate.
  #ty = 10
  ty = 15
}

function abs(x) {
  if (x < 0) return -x
  return x
}

{
  # We want to split on all of these.
  gsub(/[=,:]/, " ", $0)
  sx = $4
  sy = $6
  bx = $12
  by = $14
  r = abs(sx - bx) + abs(sy - by)

  # Compute the distance to the line at y=ty.
  tdist = abs(sy - ty)

  if (tdist <= r) {
    # If the line intersects the "circle" (for Manhattan metric), then some
    # points inside need to be marked. Compute half the width of the line
    # segment that intersects the "circle"/diamond.
    rt = r - tdist
    for (i = sx - rt; i <= sx + rt; i++) {
      marks[i] = 1
    }
  }
}

END {
  print("Number of no-beacon positions:", length(marks))
}
