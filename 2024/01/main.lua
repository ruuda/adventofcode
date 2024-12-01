#!/usr/bin/env luajit

lhs = {}
rhs = {}

f = io.open("input.txt", "r")
for line in f:lines() do
  t = lhs
  for v in string.gmatch(line, "[^ ]+") do
    table.insert(t, v)
    t = rhs
  end
end

table.sort(lhs)
table.sort(rhs)

d = 0
for i = 1, table.getn(lhs) do
  d = d + math.abs(lhs[i] - rhs[i])
end

print("Part 1:", d)

d = 0
for i = 1, table.getn(lhs) do
  n = 0
  k = lhs[i]
  for j = 1, table.getn(lhs) do
    if k == rhs[j] then
      n = n + 1
    end
  end
  d = d + k * n
end

print("Part 2:", d)

