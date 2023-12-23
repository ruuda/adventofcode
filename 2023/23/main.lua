#!/usr/bin/env luajit

f = io.open("example.txt", "r")
line = f:read()
while line do
  print(line)
  line = f:read()
end
