#!/usr/bin/awk -f

BEGIN {
  n = 0;
  prev = 0;
}

{
  n += (NR > 1 && $0 > prev);
  prev = $0;
}

END {
  print(n);
}
