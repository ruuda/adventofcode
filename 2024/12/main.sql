-- Usage: sqlite3 < main.sql

-- Import the file with numbered lines.
create table lines (plots text);
.import --csv 'input.txt' lines
create table y_lines (y integer primary key autoincrement, plots text);
insert into y_lines (plots) select plots from lines;

-- Make the output more readable during development.
.mode column

-- Expand the lines into a table with x-y coordinates and plot,
-- and give every plot a unique id.
create table map (
  id integer primary key autoincrement,
  x integer,
  y integer,
  t text
);
with xs (x) as (
  select 1
  union all
  select x + 1 from xs
  limit (select length(plots) from y_lines where y = 1)
)
insert into
  map (x, y, t)
select
  x, y, substring(plots, x, 1) as t
from
  y_lines, xs;

create index ix_map_x on map (x);
create index ix_map_y on map (y);

-- Now for every plot, we find the minimal plot id of the area that it is part
-- of, by continuing to add rows with a lower id if we find neighbors.
create table regions (id integer, t text, x integer, y integer);
with pre_regions as not materialized (
  select x, y, t, id from map
  union
  select
    a2.x, a2.y, a1.t, a1.id
  from
    pre_regions a1, map a2
  where
    (a1.t = a2.t)
    and (a1.id < a2.id)
    and (
      (a1.x + 1 = a2.x and a1.y = a2.y) or
      (a1.x - 1 = a2.x and a1.y = a2.y) or
      (a1.x = a2.x and a1.y + 1 = a2.y) or
      (a1.x = a2.x and a1.y - 1 = a2.y)
    )
)
insert into regions
select
  min(id) as id, min(t) as t, x, y
from
  pre_regions
group by
  x, y;

create index ix_regions_x_y on regions (x, y);

-- If the neighbor is a different region, then we have a fence.
with fences as (
  select
    id, x, y,
    1 - (select count(*) from regions n where n.id = m.id and n.x = m.x and n.y = m.y - 1) as n,
    1 - (select count(*) from regions e where e.id = m.id and e.x = m.x + 1 and e.y = m.y) as e,
    1 - (select count(*) from regions s where s.id = m.id and s.x = m.x and s.y = m.y + 1) as s,
    1 - (select count(*) from regions w where w.id = m.id and w.x = m.x - 1 and w.y = m.y) as w
  from
    regions m
),

-- If a fence is adjacent to a fence on the same side of the plot, of an
-- adjacent plot that is part of the same region, then we only count it if it is
-- the first in that direction.
sides as (
  select
    id,
    x, y,
    n * (1 - coalesce((select n from fences w where w.id = m.id and w.x = m.x - 1 and w.y = m.y), 0)) as n,
    s * (1 - coalesce((select s from fences w where w.id = m.id and w.x = m.x - 1 and w.y = m.y), 0)) as s,
    e * (1 - coalesce((select e from fences n where n.id = m.id and n.x = m.x and n.y = m.y - 1), 0)) as e,
    w * (1 - coalesce((select w from fences n where n.id = m.id and n.x = m.x and n.y = m.y - 1), 0)) as w
  from
    fences m
),

total_area as (
  select
    id, t, count(*) as area
  from
    regions
  group by
    id, t
),
total_perimeter as (
  select
    id, sum(n) + sum(e) + sum(s) + sum(w) as perimeter
  from
    fences
  group by
    id
),
total_sides as (
  select
    id, sum(n) + sum(e) + sum(s) + sum(w) as side_count
  from
    sides
  group by
    id
),
total_cost as (
  select
    a.id,
    a.t,
    area,
    perimeter,
    side_count,
    area * perimeter as cost_part1,
    area * side_count as cost_part2
  from
    total_area a,
    total_perimeter p,
    total_sides s
  where
    (a.id = p.id) and (a.id = s.id)
)

select sum(cost_part1) as part1, sum(cost_part2) as part2 from total_cost;
