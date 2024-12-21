#!/usr/bin/env swipl

:- initialization(main, main).

main :-
  read_plots('example1.txt'),
  region(3, 0, 'A', 1).
%listing(region/4).

% We will create a plot(X, Y, Char, Id) term per character in the input file,
% by recursively processing the file. We need to mark plot as a dynamic
% predicate.
:- dynamic plot/4.

read_plots(File) :-
  open(File, read, Stream),
  process_file(Stream, 0, 0, 0),
  close(Stream).

process_file(Stream, _, _, _) :-
  at_end_of_stream(Stream),
  !.

process_file(Stream, X, Y, N) :-
  get_char(Stream, Char),
  ( Char == end_of_file ->
      true
  ; Char == '\n' ->
      NextY is Y + 1,
      process_file(Stream, 0, NextY, N)
  ; assertz(plot(X, Y, Char, N)),
    NextX is X + 1,
    NextN is N + 1,
    process_file(Stream, NextX, Y, NextN)
  ).

% Next, we define regions of adjacent plots with ids.
region(X, Y, Type, Id) :- plot(X, Y, Type, Id).
region(X, Y, Type, Id) :-
  plot(X, Y, Type, _),
  region(Xb, Yb, Type, Id),
  ( Xb = X, Yb is Y + 1
  ; Xb = X, Yb is Y - 1
  ; Xb is X + 1, Yb = Y
  ; Xb is X - 1, Yb = Y
  ).
