import std.conv;
import std.file;
import std.stdio;
import std.string;

void main() {
  File file = File("input.txt", "r");

  // Data holds the input data, marks holds the mark status of each cell.
  string[] data;
  char[][] marks;
  uint[][] parts_map;

  // Collect the data, fill the mark map with unknowns.
  while (!file.eof()) {
    string line = strip(file.readln());
    if (line.length == 0) continue;
    data ~= line;

    char[] marks_line;
    uint[] parts_map_line;
    for (int i = 0; i < line.length; i++) {
      marks_line ~= '?';
      parts_map_line ~= 0;
    }

    marks ~= marks_line;
    parts_map ~= parts_map_line;
  }

  // For the flood fill, we need to maintain a frontier of open nodes. Do a
  // first pass over the map, and mark all the digit positions adjacent to a
  // symbol as digit.
  int[2][] frontier;
  for (int y = 0; y < data.length; y++) {
    string line = data[y];
    for (int x = 0; x < line.length; x++) {
      switch (line[x]) {
      case '.':
        marks[y][x] = '.';
        break;

      case '0': .. case '9':
        // Digits themselves are not interesting.
        break;

      default:
        // For punctuation that is not '.', the surrounding cells should be
        // marked, put them on the frontier.
        marks[y][x] = '.';
        for (int dy = -1; dy <= 1; dy++) {
          int yy = y + dy;
          if (yy < 0 || yy >= data.length) continue;
          for (int dx = -1; dx <= 1; dx++) {
            int xx = x + dx;
            if (xx < 0 || xx >= line.length) continue;
            char cell = data[yy][xx];
            if ('0' <= cell && cell <= '9') {
              frontier ~= [xx, yy];
            }
          }
        }
      }
    }
  }

  // Next we process the frontier to discover the full integers that have at
  // least one marked digit.
  for (int f = 0; f < frontier.length; f++) {
    int x = frontier[f][0];
    int y = frontier[f][1];
    if (marks[y][x] != '?') continue;
    marks[y][x] = 'd';

    foreach (dx; [-1, 1]) {
      int xx = x + dx;
      if (xx < 0 || xx >= marks[y].length) continue;
      char cell = data[y][xx];
      if ('0' <= cell && cell <= '9') {
        frontier ~= [xx, y];
      }
    }

  }

  for (int i = 0; i < data.length; i++) {
    write(data[i]);
    write("   ");
    writeln(marks[i]);
  }

  // Finally, we can walk over the marked map to identify all integers. For part
  // 2, we will also assign the part number to its location on the map.
  uint[] part_numbers;

  for (int y = 0; y < data.length; y++) {
    string line = data[y];

    for (int x = 0; x < line.length; x++) {
      if (marks[y][x] != 'd') continue;
      int xx = x + 1;
      for (; xx < line.length; xx++) {
        if (marks[y][xx] != 'd') break;
      }
      string number_str = line[x..xx];
      uint part_number = to!uint(number_str);
      part_numbers ~= part_number;

      for (int ix = x; ix < xx; ix++) {
        parts_map[y][ix] = part_number;
      }

      x = xx;
    }
  }

  uint part_number_sum = 0;
  foreach (part_number; part_numbers) {
    part_number_sum += part_number;
  }

  writeln("\nPart 1: Sum of part numbers: ", part_number_sum, "\n");

  // For part 2, we walk the map again, and identify all gears.
  uint gear_ratio_sum = 0;
  for (int y = 0; y < data.length; y++) {
    string line = data[y];
    for (int x = 0; x < line.length; x++) {
      if (line[x] != '*') continue;

      // Locate all the parts adjacent to the gear. This assumes that no gear
      // will have the same part number next to it multiple times.
      uint[] adjacent_parts;
      for (int dy = -1; dy <= 1; dy++) {
        int yy = y + dy;
        if (yy < 0 || yy >= data.length) continue;
        for (int dx = -1; dx <= 1; dx++) {
          int xx = x + dx;
          if (xx < 0 || xx > line.length) continue;

          uint part_number = parts_map[yy][xx];
          if (part_number == 0) continue;

          bool counted_part = false;
          foreach (uint ap; adjacent_parts) {
            if (ap == part_number) {
              counted_part = true;
              break;
            }
          }

          if (!counted_part) adjacent_parts ~= part_number;
        }
      }

      if (adjacent_parts.length == 2) {
        writeln("Found a gear at ", x, ", ", y, ": ", adjacent_parts);
        uint gear_ratio = adjacent_parts[0] * adjacent_parts[1];
        gear_ratio_sum += gear_ratio;
      }
    }
  }
  writeln("\nPart 2: Sum of gear ratios: ", gear_ratio_sum);
}
