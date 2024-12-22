# Run as `awk -f main.awk < example.txt`.

BEGIN {
  # Split on any of these characters.
  FS = "[=, ]"
  mode = "input"
  if (mode == "example") {
    w = 11
    h = 7
  } else {
    w = 101
    h = 103
  }
  dt = 100
  nw = 0
  ne = 0
  sw = 0
  se = 0
}

{
  x0 = $2
  y0 = $3
  vx = $5
  vy = $6
  x1 = (x0 + dt * vx) % w
  y1 = (y0 + dt * vy) % h
  if (x1 < 0) x1 += w
  if (y1 < 0) y1 += h
  if (y1 < (h - 1) / 2) {
    if (x1 < (w - 1) / 2) {
      nw += 1
    } else if (x1 >= (w + 1) / 2) {
      ne += 1
    }
  } else if (y1 >= (h + 1) / 2) {
    if (x1 < (w - 1) / 2) {
      sw += 1
    } else if (x1 >= (w + 1) / 2) {
      se += 1
    }
  }
}

END {
  print("ne", ne, "nw", nw, "sw", sw, "se", se)
  print("Part 1:", ne * nw * sw * se)
}
