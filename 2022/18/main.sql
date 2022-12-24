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

-- We do a flood fill of the exterior of the droplet, where we allow one cell
-- of space around the droplet itself. This becomes `exterior`.
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
      -- Limit the positions to our exterior cube 1 wider than the droplet.
      and e.x + d.dx >= (select min(x) - 1 from cubes)
      and e.x + d.dx <= (select max(x) + 1 from cubes)
      and e.y + d.dy >= (select min(y) - 1 from cubes)
      and e.y + d.dy <= (select max(y) + 1 from cubes)
      and e.z + d.dz >= (select min(z) - 1 from cubes)
      and e.z + d.dz <= (select max(z) + 1 from cubes)
      -- Positions that are part of the cube are not exterior.
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
-- Now count the surface area of the exterior, in the same way that we counted
-- the surface area of the droplet in part 1.
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
-- However, this counts both the inner surface of the exterior that encloses the
-- droplet, and its outer surface of the cube. We don't want this outer surface,
-- so compute its size so we can subtract it later.
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
-- The area of the droplet then, is the inner area of the exterior.
select
  sum(num_sides_exposed) - exterior_faces
from
  num_sides, exterior_size;
