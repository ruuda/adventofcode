import std.file;
import std.stdio;
import std.string;

void main() {
  File file = File("example.txt", "r");

  // Data holds the input data, marks holds the mark status of each cell.
  string[] data;
  char[][] marks;

  // Collect the data, fill the mark map with unknowns.
  while (!file.eof()) {
    string line = strip(file.readln());
    if (line.length == 0) continue;
    data ~= line;

    char[] marks_line;
    for (int i = 0; i < line.length; i++) marks_line ~= '?';
    marks ~= marks_line;
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
        // marked.
        marks[y][x] = '.';
        for (int dy = -1; dy <= 1; dy++) {
          int yy = y + dy;
          if (yy < 0 || yy >= data.length) continue;
          for (int dx = -1; dx <= 1; dx++) {
            int xx = x + dx;
            if (xx < 0 || xx >= line.length) continue;
            char cell = data[yy][xx];
            if ('0' <= cell && cell <= '9') {
              marks[yy][xx] = 'd';
              frontier ~= [xx, yy];
            }
          }
        }
      }
    }
  }

  for (int i = 0; i < data.length; i++) {
    write(data[i]);
    write("   ");
    writeln(marks[i]);
  }
}
