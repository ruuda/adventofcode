import std/sequtils
import std/strscans
import std/strutils
import std/sugar

var distances: seq[int]
var times: seq[int]

for line in "example.txt".lines:
  var raw: string

  if scanf(line, "Distance: $+", raw):
    distances = map(filter(split(raw, ' '), x => x != ""), parseInt)

  if scanf(line, "Time: $+", raw):
    times = map(filter(split(raw, ' '), x => x != ""), parseInt)

echo times, distances
