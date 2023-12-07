import std/algorithm
import std/strscans

for line in "example.txt".lines:
  var hand: string
  var bid: int

  if not scanf(line, "$+ $i", hand, bid):
    continue

  echo hand, " ", bid
