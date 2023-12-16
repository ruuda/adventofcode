// Execute with `dmd -run main.d`.

import std.conv;
import std.file;
import std.stdio;
import std.string;
import std.range.primitives;

const byte EAST = 1;
const byte NORTH = 2;
const byte WEST = 4;
const byte SOUTH = 8;

class Beam {
  int x;
  int y;
  // The beam direction is a bitmask, see constants below.
  byte dir;

  void move() {
    switch (this.dir) {
      case EAST:  this.x += 1; break;
      case NORTH: this.y -= 1; break;
      case WEST:  this.x -= 1; break;
      case SOUTH: this.y += 1; break;
      default: assert(0);
    }
  }

  Beam dup() {
    Beam b = new Beam;
    b.x = this.x;
    b.y = this.y;
    b.dir = this.dir;
    return b;
  }
}

// Implementation of part 1.
ulong count_energized(char[][] map) {
  Beam start = new Beam;
  start.x = 0;
  start.y = 0;
  start.dir = 1;
  Beam[] open = [start];

  // We maintain a parallel map about whether there is a beam incoming to a
  // tile, and in which direction it is going. The bitmask is the same as for
  // Beam.dir.
  byte[][] beams = [];
  foreach (char[] line; map) {
    byte[] beam_line;
    beam_line.length = line.length;
    beams ~= beam_line;
  }

  while (open.length > 0) {
    Beam beam = open.back();
    open.popBack();

    // Beams that fall off the map, we can ignore.
    if (beam.x < 0 || beam.x >= map[0].length) continue;
    if (beam.y < 0 || beam.y >= map.length) continue;

    // Mark the beam in our energy map if we haven't visited it before.
    if ((beams[beam.y][beam.x] & beam.dir) > 0) {
      continue;
    } else {
      beams[beam.y][beam.x] |= beam.dir;
    }

    switch (map[beam.y][beam.x]) {
      case '.':
        Beam next = beam.dup;
        next.move();
        open ~= next;
        break;
      case '\\':
        Beam next = beam.dup;
        switch (beam.dir) {
          case EAST: next.dir = SOUTH; break;
          case NORTH: next.dir = WEST; break;
          case WEST: next.dir = NORTH; break;
          case SOUTH: next.dir = EAST; break;
          default: assert(0);
        }
        next.move();
        open ~= next;
        break;
      case '/':
        Beam next = beam.dup;
        switch (beam.dir) {
          case EAST: next.dir = NORTH; break;
          case NORTH: next.dir = EAST; break;
          case WEST: next.dir = SOUTH; break;
          case SOUTH: next.dir = WEST; break;
          default: assert(0);
        }
        next.move();
        open ~= next;
        break;
      case '-':
        if (beam.dir == EAST || beam.dir == WEST) {
          // If we hit the pointy end, the beam just continues.
          Beam next = beam.dup;
          next.move();
          open ~= next;
        } else {
          // If we hit the long end, the beam splits.
          Beam next1 = beam.dup;
          next1.dir = EAST;
          next1.move();
          open ~= next1;
          Beam next2 = beam.dup;
          next2.dir = WEST;
          next2.move();
          open ~= next2;
        }
        break;
      case '|':
        if (beam.dir == NORTH || beam.dir == SOUTH) {
          // If we hit the pointy end, the beam just continues.
          Beam next = beam.dup;
          next.move();
          open ~= next;
        } else {
          Beam next1 = beam.dup;
          next1.dir = NORTH;
          next1.move();
          open ~= next1;
          Beam next2 = beam.dup;
          next2.dir = SOUTH;
          next2.move();
          open ~= next2;
        }
        break;
      default:
        assert(0);
    }
  }

  ulong result = 0;
  
  // Finally, we scan the map for locations that have a beam.
  foreach (byte[] beam_line; beams) {
    foreach (byte cell; beam_line) {
      if (cell > 0) {
        result += 1;
        write("#");
      } else {
        write(".");
      }
    }
    writeln("");
  }

  return result;
}

void main() {
  File file = File("example.txt", "r");

  // Load the input into the 'map' array.
  char[][] map;
  while (!file.eof()) {
    char[] line = strip(file.readln()).dup;
    if (line.length == 0) continue;
    map ~= line;
  }

  ulong part1 = count_energized(map);
  writeln("Part 1: ", part1);
}
