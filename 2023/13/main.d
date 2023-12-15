// Execute with `dmd -run main.d`.

import std.conv;
import std.file;
import std.stdio;
import std.string;

// This implements the computation for part 1.
ulong inspect_pattern(char[][] data, ulong skip_x, ulong skip_y) {
  ulong nx = data[0].length;
  ulong ny = data.length;

  ulong true_mx = 0;
  for (ulong mx = 1; mx < nx; mx++) {
    bool is_good = true;
    if (mx == skip_x) continue;
    for (ulong dx = 0; dx < nx - mx; dx++) {
      if (mx < dx + 1) continue;
      ulong x1 = mx - 1 - dx;
      ulong x2 = mx + dx;
      for (ulong y = 0; y < ny; y++) {
        if (data[y][x1] != data[y][x2]) {
          is_good = false;
          break;
        }
      }
      if (!is_good) break;
    }
    if (is_good) {
      true_mx = mx;
      break;
    }
  }

  ulong true_my = 0;
  for (ulong my = 1; my < ny; my++) {
    bool is_good = true;
    if (my == skip_y) continue;
    for (ulong dy = 0; dy < ny - my; dy++) {
      if (my < dy + 1) continue;
      ulong y1 = my - 1 - dy;
      ulong y2 = my + dy;
      if (data[y1] != data[y2]) {
        is_good = false;
        break;
      }
    }
    if (is_good) {
      true_my = my;
      break;
    }
  }

  return true_my * 100 + true_mx;
}

char flip(char c) {
  if (c == '.') return '#';
  if (c == '#') return '.';
  assert(0);
}

// This implements the computation for part 2.
ulong inspect_pattern_flips(char[][] data) {
  ulong res = inspect_pattern(data, 0, 0);
  // Using modular arithmetic to fake tuples ...
  ulong mx = res % 100;
  ulong my = res / 100;
  writeln("Part 1 answer: ", mx, ", ", my);

  for (ulong y = 0; y < data.length; y++) {
    for (ulong x = 0; x < data[y].length; x++) {
      char c = data[y][x];
      data[y][x] = flip(c);
      ulong flip_res = inspect_pattern(data, mx, my);
      data[y][x] = c;

      if (flip_res == 0) continue;
      writeln("Found some flip at ", x, ", ", y, " -> ", flip_res);
      ulong fmx = flip_res % 100;
      ulong fmy = flip_res / 100;
      return flip_res;
    }
  }
  assert(0);
}

void main() {
  File file = File("input.txt", "r");
  ulong result1, result2 = 0;

  // Load the pattern into this array, one by one.
  char[][] data;
  while (!file.eof()) {
    char[] line = strip(file.readln()).dup;
    if (line.length == 0) {
      result1 += inspect_pattern(data, 0, 0);
      result2 += inspect_pattern_flips(data);
      data = [];
    } else {
      data ~= line;
    }
  }

  writeln("Part 1: ", result1);
  writeln("Part 2: ", result2);
}
