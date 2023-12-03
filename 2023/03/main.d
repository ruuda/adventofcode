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

  for (int i = 0; i < data.length; i++) {
    writeln(marks[i]);
    writeln(data[i]);
  }
}
