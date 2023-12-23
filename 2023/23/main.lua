#!/usr/bin/env luajit

map = {}

f = io.open("input.txt", "r")
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

function shallow_copy(closed)
  local copy = {}
  for k, v in pairs(closed) do
    copy[k] = v
  end
  return copy
end

function explore_hike(xs, ys, closed, n_closed)
  if xs == x_end and ys == height then
    print("Found exit, route has length", n_closed)
    return n_closed
  end

  local neighbors = {
    {dx=-1, dy=0},
    {dx=0, dy=-1},
    {dx=1, dy=0},
    {dx=0, dy=1},
  }
  local candidates = {}
  for _, d in pairs(neighbors) do
    local ny = ys + d.dy
    if map[ny] ~= nil then
      local nx = xs + d.dx
      -- If we were here already we cannot go there again.
      if closed[ny * 0xffff + nx] ~= true then
        local move = map[ny][nx]
        local is_ok = (
          (move == ".")
          or (move == ">" and d.dx == 1)
          or (move == "<" and d.dx == -1)
          or (move == "^" and d.dy == -1)
          or (move == "v" and d.dy == 1)
        )
        if is_ok then
          table.insert(candidates, {x=nx, y=ny})
        end
      end
    end
  end

  if table.getn(candidates) == 0 then
    return {}
  elseif table.getn(candidates) == 1 then
    -- If there is only one step we can take, then we can do a tail call and we
    -- can recycle the closed set and mutate it in place.
    local n = candidates[1]
    closed[n.y * 0xffff + n.x] = true
    return explore_hike(n.x, n.y, closed, n_closed + 1)
  else
    -- If we are at a fork, then evaluate all possible continuations and choose
    -- the longest one.
    local longest_len = 0
    for _, n in pairs(candidates) do
      local closed_copy = shallow_copy(closed)
      closed_copy[n.y * 0xffff + n.x] = true
      local route_len = explore_hike(n.x, n.y, closed_copy, n_closed + 1)
      if route_len > longest_len then
        longest_len = route_len
      end
    end
    return longest_len
  end
end

-- Initialize n_closed to 0 instead of 1, the initial cell does not count as a
-- step.
route_len = explore_hike(x_start, 1, { [0xffff + x_start] = true }, 0)
print("Longest route has", route_len, "steps.")
