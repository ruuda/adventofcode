#!/usr/bin/gawk -f
BEGIN {
  sum = 0;
}

function reverse(x) {
  result = "";
  for (i = 0; i < length(x); i++) {
    result = result substr(x, length(x) - i, 1);
  }
  return result;
}

function as_num(x) {
  if (x == "one") { return 1; }
  if (x == "two") { return 2; }
  if (x == "three") { return 3; }
  if (x == "four") { return 4; }
  if (x == "five") { return 5; }
  if (x == "six") { return 6; }
  if (x == "seven") { return 7; }
  if (x == "eight") { return 8; }
  if (x == "nine") { return 9; }
  return x;
}

{
  regexp = "one|two|three|four|five|six|seven|eight|nine";
  match($0, "[0-9]|" regexp, pat_start);
  match(reverse($0), "[0-9]|" reverse(regexp), pat_end);
  sum += as_num(pat_start[0]) as_num(reverse(pat_end[0]));
}
END {
  print(sum);
}
