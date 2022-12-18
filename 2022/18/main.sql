-- Usage: sqlite3 < main.sql

.mode csv

create table cubes (x integer, y integer, z integer);
create index ix_cubes_x on cubes (x);
create index ix_cubes_y on cubes (y);
create index ix_cubes_z on cubes (z);

.import 'input.txt' cubes

.print "Part 1 answer:"
with num_sides as (
  select
    6
    - (select count(*) from cubes c2 where abs(c1.x - c2.x) = 1 and c1.y = c2.y and c1.z = c2.z)
    - (select count(*) from cubes c2 where abs(c1.y - c2.y) = 1 and c1.z = c2.z and c1.x = c2.x)
    - (select count(*) from cubes c2 where abs(c1.z - c2.z) = 1 and c1.x = c2.x and c1.y = c2.y)
    as num_sides_exposed
  from
    cubes c1
)
select sum(num_sides_exposed) from num_sides;

create table deltas
  ( dx integer
  , dy integer
  , dz integer
  );
insert into deltas values
  (-1,  0,  0),
  ( 1,  0,  0),
  ( 0, -1,  0),
  ( 0,  1,  0),
  ( 0,  0, -1),
  ( 0,  0,  1)
;

.print "Part 2 answer:"
with recursive exterior (x, y, z) as (
    values (
      (select min(x) - 1 from cubes),
      (select min(y) - 1 from cubes),
      (select min(z) - 1 from cubes)
    )
  union
    select
      e.x + d.dx as x,
      e.y + d.dy as y,
      e.z + d.dz as z
    from
      exterior as e,
      deltas as d
    where
      true
      and e.x + d.dx >= (select min(x) - 1 from cubes)
      and e.x + d.dx <= (select max(x) + 1 from cubes)
      and e.y + d.dy >= (select min(y) - 1 from cubes)
      and e.y + d.dy <= (select max(y) + 1 from cubes)
      and e.z + d.dz >= (select min(z) - 1 from cubes)
      and e.z + d.dz <= (select max(z) + 1 from cubes)
      and not exists (
        select
          1
        from
          cubes as c
        where 
          true
          and e.x + d.dx = c.x
          and e.y + d.dy = c.y
          and e.z + d.dz = c.z
      )
),
num_sides as (
  select
    6
    - (select count(*) from exterior c2 where abs(c1.x - c2.x) = 1 and c1.y = c2.y and c1.z = c2.z)
    - (select count(*) from exterior c2 where abs(c1.y - c2.y) = 1 and c1.z = c2.z and c1.x = c2.x)
    - (select count(*) from exterior c2 where abs(c1.z - c2.z) = 1 and c1.x = c2.x and c1.y = c2.y)
    as num_sides_exposed
  from
    exterior c1
),
exterior_size as (
  select
    0
    + 2 * (1 + max(x) - min(x)) * (1 + max(y) - min(y))
    + 2 * (1 + max(y) - min(y)) * (1 + max(z) - min(z))
    + 2 * (1 + max(z) - min(z)) * (1 + max(x) - min(x))
    as exterior_faces
  from
    exterior
)
select
  sum(num_sides_exposed) - exterior_faces
from
  num_sides, exterior_size;
