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
  # Sensor posision
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
  for (x = 0; x <= maxcoord; x++) {
    print("x =", x)
    for (y = 0; y <= maxcoord; y++) {
      for (i = 0; i < n; i++) {
        sx = sensor_x[i]
        sy = sensor_y[i]
        r = sensor_r[i]
        d = abs(sx - x) + abs(sy - y)
        if (d <= r) {
          # The beacon couldn't be here, because there is one closer to the
          # sensor.
          break
        }
      }
      if (i == n) {
        print("Beacon could be at", x, y)
        print("Tuning frequency is", x * 4000000 + y)
      }
    }
  }
}
