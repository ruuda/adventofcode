#!/usr/bin/awk -f

BEGIN {
  hpos = 0;
  depth = 0;
}

$1 == "forward" { hpos  += $2; }
$1 == "up"      { depth -= $2; }
$1 == "down"    { depth += $2; }

END {
  print(hpos * depth);
}
