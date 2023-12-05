import std/strscans
import std/strutils
import std/sequtils

for line in "example.txt".lines:
  var dst_start, src_start, range_len: int
  var seeds_str, map_name: string
  var seeds: seq[int]

  if line == "":
    continue

  elif scanf(line, "seeds: $+", seeds_str):
    let seeds = map(split(seeds_str, ' '), parseInt)

  elif scanf(line, "$+ map:", map_name):
    echo "found map: ", line

  elif scanf(line, "$i $i $i", dst_start, src_start, range_len):
    echo "Found mapping", dst_start, src_start, range_len

  else:
    echo "bad line", line
    raise newException(Defect, "Unexpected line.")
