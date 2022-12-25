#!/usr/bin/awk -f
# Pipe the input txt to this program.

BEGIN {
  sum = 0
}

{
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

END {
  print(sum)
}
