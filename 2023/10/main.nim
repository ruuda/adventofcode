import std/sequtils
import std/strscans
import std/strutils

var maze: seq[string] = @[]
var sx, sy: int

for line in "example1.txt".lines:
  if line == "":
    continue

  # Append the line with a buffer cell on the sides, so we don't have to worry
  # about index out of bounds later.
  let line_wide = "." & line & "."
  maze.add(line_wide)

  let si = line_wide.find("S")
  if si != -1:
    sx = si
    sy = maze.len

# We also need to add padding at the top and bottom.
let line_pad = '.'.repeat(maze[0].len)
maze.insert(line_pad, 0)
maze.add(line_pad)

echo sx, sy
for line in maze:
    echo line
