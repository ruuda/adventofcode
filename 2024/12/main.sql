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
create index ix_map_x on map (x);
create index ix_map_y on map (y);
with xs (x) as (
  select 1
  union all
  select x + 1 from xs
  limit (select length(plots) from y_lines where y = 1)
)
insert into
  map (x, y, t)
select
  y, x, substring(plots, x, 1) as t
from
  y_lines, xs;

-- Now for every plot, we find the minimal plot id of the area that it is part
-- of, by continuing to add rows with a lower id if we find neighbors.
with areas as (
  select
    id, x, y, t
  from
    map
  union all
  select
    a1.id, a2.x, a2.y, a1.t
  from
    areas a1,
    map a2
  where
    (a1.t = a2.t)
    and ((a1.x + 1 = a2.x and a1.y = a2.y) or (a1.y + 1 = a2.y and a1.x = a2.x))
),
merged_plots as (
  select
    min(id) as id,
    min(t) as t,
    x,
    y
  from
    areas
  group by
    x, y
)

select
  id, count(*), min(t)
from
  merged_plots
group by
  id;
