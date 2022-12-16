#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  FS = " "
  # Target row to investigate.
  ty = 10 # For the example.
  ty = 2000000
}

function abs(x) {
  if (x < 0) return -x
  return x
}

{
  # We want to split on all of these.
  gsub(/[=,:]/, " ", $0)
  # Sensor posision
  sx = $4
  sy = $6
  # Beacon position
  bx = $12
  by = $14
  # "Radius" of the "circle"
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

  # We also need to count if there are any beacons already on the line -- these
  # don't count as no-beacon positions.
  if (by == ty) {
    beacons[bx] = 1
  }
}

END {
  print("Number of no-beacon positions:", length(marks) - length(beacons))
}
