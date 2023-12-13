// Execute with `dmd -run main.d`.

import std.conv;
import std.file;
import std.stdio;
import std.string;

ulong inspect_pattern(string[] data) {
  ulong nx = data[0].length;
  ulong ny = data.length;

  ulong true_mx = 0;
  for (ulong mx = 1; mx < nx; mx++) {
    bool is_good = true;
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

  writeln("mx: ", true_mx);
  writeln("my: ", true_my);

  return true_my * 100 + true_mx;
}

void main() {
  File file = File("input.txt", "r");
  ulong result = 0;

  // Load the pattern into this array, one by one.
  string[] data;
  while (!file.eof()) {
    string line = strip(file.readln());
    if (line.length == 0) {
      result += inspect_pattern(data);
      data = [];
    } else {
      data ~= line;
    }
  }

  writeln("\nPart 1: ", result);
}
