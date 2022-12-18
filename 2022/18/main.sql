-- Usage: sqlite3 < main.sql

.mode csv

create table cubes (x integer, y integer, z integer);

.import 'input.txt' cubes

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
