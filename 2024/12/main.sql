-- Usage: sqlite3 < main.sql

-- Import the file with numbered lines.
create table lines (plots text);
.import --csv 'example1.txt' lines
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

-- Now for every plot, we find the minimal plot id of the area that it is part
-- of, by continuing to add rows with a lower id if we find neighbors.
with areas as (
  select x, y, t, id from map
  union all
  select
    case when a1.id < a2.id then a1.x else a2.x end as x,
    case when a1.id < a2.id then a1.y else a2.y end as y,
    a1.t,
    case when a1.id < a2.id then a1.id else a2.id end as id
  from
    areas a1, map a2
  where
    (a1.t = a2.t)
    and (
      (a1.x + 1 = a2.x and a1.y = a2.y) or
      (a1.x = a2.x and a1.y + 1 = a2.y)
    )
),
merged_plots as (
  select
    min(id) as id, min(t) as t, x, y
  from
    areas
  group by
    x, y
),
fences as (
  select
    id,
    -- If the neighbor is a different merged plot, then we have a fence.
    4
    - (select count(*) from merged_plots n where n.id = m.id and n.x = m.x and n.y = m.y - 1)
    - (select count(*) from merged_plots e where e.id = m.id and e.x = m.x + 1 and e.y = m.y)
    - (select count(*) from merged_plots s where s.id = m.id and s.x = m.x and s.y = m.y + 1)
    - (select count(*) from merged_plots w where w.id = m.id and w.x = m.x - 1 and w.y = m.y)
    as n_fences
  from
    merged_plots m
),
total_area as (
  select
    id, t, count(*) as area
  from
    merged_plots
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

select * from total_cost;
