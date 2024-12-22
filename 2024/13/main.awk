# Run as `awk -f main.awk < example.txt`.
BEGIN {
  total_cost = 0;
  part = 2;
}
$2 == "A:" {
  dx_a = substr($3, 3, length($3) - 2)
  dy_a = substr($4, 3)
}
$2 == "B:" {
  dx_b = substr($3, 3, length($3) - 2)
  dy_b = substr($4, 3)
}
$1 == "Prize:" {
  px = substr($2, 3, length($2) - 2);
  py = substr($3, 3);
  if (part == 2) {
    px += 10000000000000;
    py += 10000000000000;
  }

  # The vectors A = (dx_a, dy_a) and B = (dx_b, dy_b) define a basis in which
  # we need to express P = (px, py). We can use the standard formula for
  # barycentric coordinates to express P as a point on the triangle spanned by
  # the origin and A and B:
  a = (dy_b * px - dx_b * py) / (dy_b * dx_a - dx_b * dy_a);
  b = (dx_a * py - dy_a * px) / (dy_b * dx_a - dx_b * dy_a);
  if (int(a) == a && int(b) == b) {
    if (part == 2 || (a <= 100 && b <= 100)) total_cost += 3 * a + b;
  }
}
END {
  print(total_cost);
}
