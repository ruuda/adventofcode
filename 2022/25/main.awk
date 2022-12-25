#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  sum = 0
}

{
  # Convert from SNAFU to decimal numbers.
  v = 0
  for (i = 1; i <= length($0); i++) {
    ch = substr($0, i, 1)
    v = v * 5
    if (ch == "=") { v -= 2 }
    if (ch == "-") { v -= 1 }
    if (ch == "1") { v += 1 }
    if (ch == "2") { v += 2 }
  }
  print(v)
  sum += v
}

# Render a number in SNAFU notation.
function snafu(x) {
  w = ""
  while (x > 0) {
    r = x % 5
    if (r == 0) { w = "0" w }
    if (r == 1) { w = "1" w }
    if (r == 2) { w = "2" w }
    if (r == 3) { w = "=" w; x += 5 }
    if (r == 4) { w = "-" w; x += 5 }
    x = int(x / 5)
  }
  return w
}

END {
  print("Decimal: " sum ", SNAFU: " snafu(sum))
}
