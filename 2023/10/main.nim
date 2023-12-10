import std/heapqueue
import std/sequtils
import std/strutils
import std/sugar

var maze: seq[string] = @[]
var sx, sy: int

for line in "input.txt".lines:
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

iterator endpoints(cell: char): (int, int) =
  case cell:
    of '|':
      yield (0, -1)
      yield (0, 1)
    of '-':
      yield (-1, 0)
      yield (1, 0)
    of 'L':
      yield (0, -1)
      yield (1, 0)
    of 'J':
      yield (0, -1)
      yield (-1, 0)
    of '7':
      yield (-1, 0)
      yield (0, 1)
    of 'F':
      yield (1, 0)
      yield (0, 1)
    of 'S':
      yield (-1, 0)
      yield (1, 0)
      yield (0, -1)
      yield (0, 1)
    of '.':
      let _ = "Nothing to yield, I don't know how to express that."
    else:
      raise newException(Defect, "Unsupported input cell.")

# Build a parallel grid that holds the distance from the center along the maze.
var distances: seq[seq[int]] = maze.map(line => line.map(_ => -1))
var max_dist: int = 0
distances[sy][sx] = 0

# Keep a frontier of open nodes to explore. We initialize it with all the nodes
# reachable from the start. These are not just the neighbors of the start, they
# also need to connect back!
var open: HeapQueue[(int, int, int)] = initHeapQueue[(int, int, int)]()
for dx, dy in endpoints('S'):
  let cell = maze[sy + dy][sx + dx]
  for ddx, ddy in endpoints(cell):
    if (dx + ddx) == 0 and (dy + ddy) == 0:
      open.push((1, sx + dx, sy + dy))

echo "Initial nodes: ", $open

while open.len > 0:
  let (d, x, y) = open.pop()
  if distances[y][x] != -1:
    # We have already visited this node.
    continue

  distances[y][x] = d
  if d > max_dist:
    max_dist = d

  for dx, dy in endpoints(maze[y][x]):
    open.push((d + 1, x + dx, y + dy))

# Print the distances, just for visual debugging.
for line in distances:
  for d in line:
    if d == -1:
      stdout.write('.')
    elif d <= 9:
      stdout.write(d)
    else:
      stdout.write('X')
  stdout.write('\n')

echo "Max distance: ", max_dist
