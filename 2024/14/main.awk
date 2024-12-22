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
  n = 0
}

{
  x0 = $2
  y0 = $3
  vx = $5
  vy = $6
  n++
  ax[n] = x0; ay[n] = y0; avx[n] = vx; avy[n] = vy;
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

  # Zoom out a bit to draw a picture in the terminal. Choose a different scale
  # for x and y to compensate for the elongated characters.
  sx = 3
  sy = 6

  for (t = 0; t < 10000; t++) {
    picture = "";
    entropy = 0;
    prev_n = 0;
    for (y = 0; y < h; y += sy) {
      for (x = 0; x < w; x += sx) {
        n_robots = 0;
        for (i = 1; i <= n; i++) {
          if ((ax[i] >= x) && (ax[i] < x + sx) && (ay[i] >= y) && (ay[i] < y + sy)) {
            n_robots += 1;
          }
        }
        if (n_robots > 1) {
          picture = picture "@"
        } else if (n_robots == 1) {
          picture = picture "."
        } else {
          picture = picture " "
        }
        # Crude heuristic for how "random" the picture is, so we draw only the
        # interesting ones.
        entropy += (prev_n > 0) != (n_robots > 0);
        prev_n = n_robots;
      }
      picture = picture "\n"
    }
    if (entropy < 230) {
      print("\n\nt =", t, "entropy =", entropy)
      print(picture)
    }
    for (i = 1; i <= n; i++) {
      ax[i] = (ax[i] + avx[i]) % w;
      ay[i] = (ay[i] + avy[i]) % h;
      if (ax[i] < 0) ax[i] += w;
      if (ay[i] < 0) ay[i] += h;
    }
  }
}
