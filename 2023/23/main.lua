#!/usr/bin/env luajit

map = {}

f = io.open("example.txt", "r")
for line in f:lines() do
  map_line = {}
  for i = 1, line:len() do
    table.insert(map_line, line:sub(i, i))
  end
  table.insert(map, map_line)
end

-- Locate the start and end of the maze.
x_start = 0
x_end = 0
height = table.getn(map)
for j = 1, table.getn(map[1]) do
  if map[1][j] == "." then
    x_start = j
    break
  end
end
for j = 1, table.getn(map[height]) do
  if map[height][j] == "." then
    x_end = j
    break
  end
end
print("x_start", x_start)
print("x_end", x_end)
