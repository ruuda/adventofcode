-- Usage: sqlite3 < main.sql

-- Import the file with numbered lines.
create table lines (heights text);
.import --csv 'example1.txt' lines
create table y_lines (y integer primary key autoincrement, heights text);
insert into y_lines (heights) select heights from lines;

select * from y_lines;
