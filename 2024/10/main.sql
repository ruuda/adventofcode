-- Usage: sqlite3 < main.sql

-- Import the file with numbered lines.
create table lines (heights text);
.import --csv 'input.txt' lines
create table y_lines (y integer primary key autoincrement, heights text);
insert into y_lines (heights) select heights from lines;

-- Expand the lines into a table with x-y coordinates and heights.
create table topomap (x integer, y integer, h integer);
create index ix_topomap_x on topomap (x);
create index ix_topomap_y on topomap (y);
with xs (x) as (
  select 1
  union all
  select x + 1 from xs
  limit (select length(heights) from y_lines where y = 1)
)
insert into
  topomap (x, y, h)
select
  y, x, substring(heights, x, 1) as h
from
  y_lines, xs;

-- Find all the start positions.
create table trailheads (x integer, y integer);
insert into trailheads (x, y) select x, y from topomap where h = 0;

-- Build a table of all trail steps. Trail steps are identified by their
-- position, the trailhead they started at, and they have a height.
with trails (hx, hy, x, y, h) as (
  select
    h.x as hx, h.y as hy, h.x as x, h.y as y, m.h as h
  from
    trailheads h, topomap m
  where
    m.x = h.x and m.y = h.y
  union 
  select
    t.hx, t.hy, m.x as x, m.y as y, m.h as h
  from
    trails t, topomap m
  where
    (m.h = t.h + 1) and (
      false
      or ((t.x = m.x + 1) and (t.y = m.y))
      or ((t.x = m.x - 1) and (t.y = m.y))
      or ((t.x = m.x) and (t.y = m.y + 1))
      or ((t.x = m.x) and (t.y = m.y - 1))
    )
),
trail_scores as (
  select
    hx, hy, count(*) as score
  from
    trails
  where
    h = 9
  group by
    hx, hy
)
select sum(score) from trail_scores;
