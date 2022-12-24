#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  FS = " "
  # For the example.
  # The beacon may be located in [0, maxcoord] (both x and y).
  maxcoord = 20

  # For the actual input.
  maxcoord = 4000000

  n = 0
}

function abs(x) {
  if (x < 0) return -x
  return x
}

{
  # We want to split on all of these.
  gsub(/[=,:]/, " ", $0)
  # Sensor position
  sx = $4
  sy = $6
  # Beacon position
  bx = $12
  by = $14
  # "Radius" of the "circle"
  r = abs(sx - bx) + abs(sy - by)
  sensor_x[n] = sx
  sensor_y[n] = sy
  sensor_r[n] = r
  n++
}

END {
  for (y = 0; y <= maxcoord; y++) {
    if (y % 10000 == 0) { print("y =", y) }
    for (x = 0; x <= maxcoord; x++) {
      for (i = 0; i < n; i++) {
        sx = sensor_x[i]
        sy = sensor_y[i]
        r = sensor_r[i]
        d = abs(sx - x) + abs(sy - y)
        if (d <= r) {
          # The beacon couldn't be here, because there is one closer to the
          # sensor. We know the radius of the "circle", so we can already
          # advance x, we don't need to continue to scan pointlessly.
          tdist = abs(sy - y)
          rt = r - tdist
          if (sx + rt > x) {
            x = sx + rt
          }
          break
        }
      }

      # If i == n, then we inspected all sensors, and we did not bail out, which
      # means this position is left over as valid.
      if (i == n) {
        print("Beacon could be at", x, y)
        print("Tuning frequency is", x * 4000000 + y)
      }
    }
  }
}
