import std/sequtils
import std/strscans
import std/strutils
import std/sugar
import std/math

var distances: seq[int]
var times: seq[int]

let part = 2

for line in "input.txt".lines:
  var raw: string

  if scanf(line, "Distance: $+", raw):
    if part == 1:
      distances = map(filter(split(raw, ' '), x => x != ""), parseInt)
    else:
      distances = @[parseInt(replace(raw, " ", ""))]

  if scanf(line, "Time: $+", raw):
    if part == 1:
      times = map(filter(split(raw, ' '), x => x != ""), parseInt)
    else:
      times = @[parseInt(replace(raw, " ", ""))]

var result_product = 1.0

for (time, distance) in zip(times, distances):
  # How many ways are there to win the race? The distance covered for a race of
  # duration t ms where we press the button for h ms is h * (t - h) = ht - h².
  # Its derivative w.r.t. h is t - 2h, so this has an optimum at h = t/2. But
  # that's not relevant in this case, we want to count all the hs where the
  # distance is above the record d. So we solve for ht - h² = d to find extreme
  # values of h, we get h = (1/2)(t ± sqrt(t² - 4d)).
  let h_min = 0.5 * (time.toFloat() - sqrt((time * time - 4 * distance).toFloat()));
  let h_max = 0.5 * (time.toFloat() + sqrt((time * time - 4 * distance).toFloat()));
  # Note, we can't just use floor and ceil, because if the resulting bounds are
  # integers, we have to _exceed_ them to beat the record. It's not about
  # rounding up and down to the next integer, it's the next integer
  # after/before.
  let num_wins = 1 + (ceil(h_max) - 1) - (floor(h_min) + 1);
  result_product *= num_wins
  echo "min: ", h_min, " max: ", h_max, " num_wins: ", num_wins

echo result_product
