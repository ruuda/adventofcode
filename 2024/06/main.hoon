!:
=/  input=cord
'''
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
'''
=/  lines  (to-wain:format input)
::  locate the start position (always ^)
::  first we produce ~ per line that doesn't contain the cursor,
::  or [x y] for the line that does, then we pick that out with
::  a roll
::
=/  startpos  %+  roll
  =<  p
  %^  spin  lines  0
  |=  [line=cord i=@ud]
  :_  .+  i
  =/  j  (find "^" (trip line))
  ?^  j  :_
      i 
    =<  u  j
  ~
|=  [p1=noun p2=noun]
?^  p1  p1  p2
startpos
