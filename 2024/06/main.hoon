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
::  first we produce 'n' per line that doesn't contain the cursor,
::  or [x y] for the line that does
%^  spin  lines  0
|=  [line=cord i=@ud]
:_  .+  i
=/  j  (find "^" (trip line))
?^  j  :_
    i 
  =<  u  j
'n'
