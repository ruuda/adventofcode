#!/usr/bin/awk -f

BEGIN {
  aim   = 0;
  hpos  = 0;
  depth = 0;
}

$1 == "forward" { hpos += $2; depth += aim * $2; }
$1 == "up"      { aim -= $2; }
$1 == "down"    { aim += $2; }

END {
  print(hpos * depth);
}
