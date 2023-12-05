import std/sequtils
import std/strscans
import std/strutils
import std/sugar

# Which puzzle are we solving, part 1 or part 2?
let part = 2

type Range = object
  begin: int
  past_end: int

func initRange(begin: int, past_end: int): Range =
  result.begin = begin
  result.past_end = past_end

# Return mapped and unmapped ranges.
func moveRange(in_range: Range, map_range: Range, offset: int): (seq[Range], seq[Range]) =
  var mapped: seq[Range] = @[]
  var unmapped: seq[Range] = @[]

  # No overlap.
  if in_range.past_end <= map_range.begin:
    unmapped.add(in_range)

  elif in_range.begin >= map_range.past_end:
    unmapped.add(in_range)

  # The inner range is contained so we map the entire thing.
  elif in_range.begin >= map_range.begin and in_range.past_end <= map_range.past_end:
    mapped.add(initRange(in_range.begin + offset, in_range.past_end + offset))

  # The map range is contained, so we have to slice it out.
  elif map_range.begin >= in_range.begin and map_range.past_end <= in_range.past_end:
    unmapped.add(initRange(in_range.begin, map_range.begin))
    unmapped.add(initRange(map_range.past_end, in_range.past_end))
    mapped.add(initRange(map_range.begin + offset, map_range.past_end + offset))

  # The map range overlaps the left-hand side.
  elif map_range.begin <= in_range.begin:
    assert map_range.past_end <= in_range.past_end
    mapped.add(initRange(in_range.begin + offset, map_range.past_end + offset))
    unmapped.add(initRange(map_range.past_end, in_range.past_end))

  # The map range overlaps the right-hand side.
  elif map_range.begin >= in_range.begin:
    unmapped.add(initRange(in_range.begin, map_range.begin))
    mapped.add(initRange(map_range.begin + offset, in_range.past_end + offset))

  else:
    raise newException(Defect, "Cases are exhaustive.")

  return (mapped, unmapped)

var seeds: seq[Range] = @[]
var mapped_seeds: seq[Range] = @[]

for line in "input.txt".lines:
  var dst_start, src_start, range_len: int
  var seeds_str, map_name: string

  if line == "":
    continue

  elif scanf(line, "seeds: $+", seeds_str):
    let raw = map(split(seeds_str, ' '), parseInt)
    if part == 1:
      for i in raw:
        mapped_seeds.add(initRange(i, i + 1))

    if part == 2:
      for i in countup(0, raw.len - 1, 2):
        mapped_seeds.add(initRange(raw[i], raw[i] + raw[i + 1]))

  elif scanf(line, "$+ map:", map_name):
    # When a new table starts, we map the currently unmapped seeds as-is.
    seeds = seeds & mapped_seeds
    mapped_seeds = @[]
    echo "Mapped leftover ", seeds

  elif scanf(line, "$i $i $i", dst_start, src_start, range_len):
    var leftover: seq[Range] = @[]
    while seeds.len > 0:
      let in_range = seeds.pop()
      let src_range = initRange(src_start, src_start + range_len)
      let offset = dst_start - src_start
      let (mapped_range, unmapped_range) = moveRange(in_range, src_range, offset)
      mapped_seeds = mapped_seeds & mapped_range
      leftover = leftover & unmapped_range
    seeds = leftover

  else:
    raise newException(Defect, "Unexpected line.")

# Finally, there may be unmapped seeds that we map as-is.
seeds = seeds & mapped_seeds
echo "Mapped final ", seeds

# We need to remove empty ranges.
seeds = filter(seeds, sr => sr.past_end > sr.begin)

# "seeds" now contains the "locations", find the minimal index.
var minloc = seeds[0].begin
for sr in seeds:
  if sr.begin < minloc:
    minloc = sr.begin

echo "Minimal location: ", minloc
