import std/heapqueue
import std/sequtils
import std/strutils
import std/strformat
import std/sugar

var maze: seq[string] = @[]
var sx, sy: int

for line in "example4.txt".lines:
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
var sdx, sdy: int = 0
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

# For part 2, we are going to classify every cell into one of 2^4 cases, one
# bit for each quadrant, which is 0 for outside and 1 for inside. The mask is:
# +-------------+
# | 0001 | 0010 |
# |------+------|
# | 0100 | 1000 |
# +-------------+

# Extract the component from the 5 quarter cells adjacent to this cell. All the
# quarter cells that are marked in the mask must belong to the same component,
# this function asserts that. The mask goes from top right in the
# least-significant bit to bottom-left in the most significant bit. Take the
# component, and set every bit of the `into` mask to that component, and every
# complementary bit to the complementary component.
func getComponent(mask: uint8, into: uint8, left: uint8, top_left: uint8, top: uint8): uint8 =
  let bits = (
    ((top and 0b1000) shr 3) or
    ((top and 0b0100) shr 1) or
    ((top_left and 0b1000) shr 1) or
    ((left and 0b0010) shl 2) or
    ((left and 0b1000) shl 1)
  )
  # Adjacent quarter cells must belong to a single component, so the collected
  # bits should be equal to the mask, or equal to its complement.
  if bits == mask:
    into
  elif bits == ((not mask) and 0b11111):
    not into
  else:
    raise newException(
      Defect,
      fmt"Component is not consistent among marked quarter cells: mask:{mask:05b} bits:{bits:05b}"
    )

# We start out by marking everything as outside, because the outside ring that
# we added is definitely outside, then we can go from there.
var component_map: seq[seq[uint8]] = maze.map(line => line.map(_ => 0u8))
for y in countup(1, maze.len - 2):
  for x in countup(1, maze[0].len - 2):
    let left = component_map[y][x - 1]
    let top_left = component_map[y - 1][x - 1]
    let top = component_map[y - 1][x]

    if distances[y][x] == -1:
      # If there is no distance in this cell, then it's not part of the main
      # loop, and it does not affect inside/outside. The component has to be the
      # same all around.
      component_map[y][x] = getComponent(0b11111, 0b1111, left, top_left, top)
      continue

    # If this cell *is* part of the main loop, then it forms a boundary of the
    # component.
    case maze[y][x]:
      of 'F':
        component_map[y][x] = getComponent(0b11_1_11, 0b0111, left, top_left, top)
      of '-':
        component_map[y][x] = getComponent(0b01_1_11, 0b0011, left, top_left, top)
      of '7':
        component_map[y][x] = getComponent(0b01_1_11, 0b1011, left, top_left, top)
      of '|':
        component_map[y][x] = getComponent(0b11_1_10, 0b0101, left, top_left, top)
      of 'J':
        component_map[y][x] = getComponent(0b01_1_10, 0b0001, left, top_left, top)
      of 'L':
        component_map[y][x] = getComponent(0b11_1_10, 0b1101, left, top_left, top)
      else:
        raise newException(Defect, fmt"This should not happen.")

echo "Max distance: ", max_dist
