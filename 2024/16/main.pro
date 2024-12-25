#!/usr/bin/env swipl

% I don't know how to make this file executable with a default goal, I wasted
% hours trying to figure out how to use `initialization`, so for now, just run
% this as `swipl main.pro` and then type the goal on the CLI.

% We will create a wall(X, Y) term per wall, and also start(X, Y) and end(X, Y)
% terms, by recursively processing the file. We need to mark those three as
% dynamic predicates.
:- dynamic wall/2.
:- dynamic start/2.
:- dynamic end/2.

read_maze(File) :-
  open(File, read, Stream),
  process_file(Stream, 0, 0),
  close(Stream).

process_file(Stream, _, _) :-
  at_end_of_stream(Stream),
  !.

process_file(Stream, X, Y) :-
  get_char(Stream, Char),
  ( Char == end_of_file ->
      true
  ; Char == '\n' ->
      NextY is Y + 1,
      process_file(Stream, 0, NextY)
  ; Char == '#' ->
      assertz(wall(X, Y)),
      NextX is X + 1,
      process_file(Stream, NextX, Y)
  ; Char == 'S' ->
      assertz(start(X, Y)),
      NextX is X + 1,
      process_file(Stream, NextX, Y)
  ; Char == 'E' ->
      assertz(end(X, Y)),
      NextX is X + 1,
      process_file(Stream, NextX, Y)
  ; NextX is X + 1,
    process_file(Stream, NextX, Y)
  ).

:- read_maze('example1.txt').

% Define how rotation works.
angles(west, north, east).
angles(south, west, north).
angles(east, south, west).
angles(north, east, south).
rotate_cw(X, Y) :- angles(_, X, Y).
rotate_ccw(X, Y) :- angles(Y, X, _).

% The start position is reachable at score 0.
reachable(X, Y, east, 0) :- start(X, Y).

reachable(X, Y, Dir, NewCost) :-
  reachable(X, Y, DirCw, Cost),
  rotate_cw(Dir, DirCw),
  NewCost is Cost + 1000.
