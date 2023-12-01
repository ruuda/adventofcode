#!/usr/bin/gawk -f
BEGIN {
  sum = 0;
}
{
  match($0, "^[^0-9]*([0-9])", pat_start);
  match($0, "([0-9])[^0-9]*$", pat_end);
  number = pat_start[1] pat_end[1];
  sum += number;
}
END {
  print(sum);
}
