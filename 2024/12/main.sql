-- Usage: sqlite3 < main.sql

-- Import the file with numbered lines.
create table lines (plots text);
.import --csv 'example3.txt' lines
create table y_lines (y integer primary key autoincrement, plots text);
insert into y_lines (plots) select plots from lines;

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
),
regions as materialized (
  select
    min(id) as id, min(t) as t, x, y
  from
    pre_regions
  group by
    x, y
),
fences as (
  select
    id,
    -- If the neighbor is a different region, then we have a fence.
    4
    - (select count(*) from regions n where n.id = m.id and n.x = m.x and n.y = m.y - 1)
    - (select count(*) from regions e where e.id = m.id and e.x = m.x + 1 and e.y = m.y)
    - (select count(*) from regions s where s.id = m.id and s.x = m.x and s.y = m.y + 1)
    - (select count(*) from regions w where w.id = m.id and w.x = m.x - 1 and w.y = m.y)
    as n_fences
  from
    regions m
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
    id, sum(n_fences) as perimeter
  from
    fences
  group by
    id
),
total_cost as (
  select
    a.id, a.t, area, perimeter, area * perimeter as cost
  from
    total_area a,
    total_perimeter p
  where
    a.id = p.id
)

select sum(cost) from total_cost;
