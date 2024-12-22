# Run as `awk -f main.awk < example.txt`.

BEGIN {
  # Split on any of these characters.
  FS = "[=, ]"
  mode = "example"
  if (mode == "example") {
    w = 11
    h = 7
  } else {
    w = 101
    h = 103
  }
  dt = 100
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
  print(x, y, vx, vy, "->", x1, y1)
}

