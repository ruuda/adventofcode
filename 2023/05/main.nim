import std/sequtils
import std/strscans
import std/strutils

var seeds: seq[int] = @[]
var mapped_seeds: seq[int]

for line in "input.txt".lines:
  var dst_start, src_start, range_len: int
  var seeds_str, map_name: string

  if line == "":
    continue

  elif scanf(line, "seeds: $+", seeds_str):
    mapped_seeds = map(split(seeds_str, ' '), parseInt)

  elif scanf(line, "$+ map:", map_name):
    # When a new table starts, we map the currently unmapped seeds as-is.
    seeds = seeds & mapped_seeds
    mapped_seeds = @[]
    echo "Mapped leftover ", seeds

  elif scanf(line, "$i $i $i", dst_start, src_start, range_len):
    var leftover: seq[int] = @[]
    for i in seeds:
      if i >= src_start and i < src_start + range_len:
        let j = i - src_start + dst_start
        mapped_seeds.add(j)
        echo "Map ", i, " -> ", j
      else:
        leftover.add(i)
    seeds = leftover

  else:
    raise newException(Defect, "Unexpected line.")

# Finally, there may be unmapped seeds that we map as-is.
seeds = seeds & mapped_seeds
echo "Mapped final ", seeds

let i_min = minIndex(seeds)
echo "Minimal location: ", seeds[i_min]
