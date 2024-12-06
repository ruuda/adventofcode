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
::  locate the start position, spos, (always ^)
::  first we produce ~ per line that doesn't contain the cursor,
::  or [x y] for the line that does, then we pick that out with
::  a roll
::
=/  spos
%-  need
%+  roll
  =<  p
  %^  spin  lines  0
  |=  [line=cord y=@ud]
  :_  .+  y
  =/  x  (find "^" (trip line))
  ^-  (unit (pair @ud @ud))
  ?^  x  [~ [u.x y]]  ~
|=  [p1=(unit (pair @ud @ud)) p2=(unit (pair @ud @ud))]
?^  p1  p1  p2
::  define a gate to look up whether there is an obstacle at a given position
::
=/  obst
|=  [x=@ud y=@ud]  =('x' (snag x (trip (snag y lines))))
::  check how big the map is
=/  mapw  (met 3 (snag 0 lines))
=/  maph  (lent lines)
::  find next position
=/  npos
|=  [x=@ud y=@ud d=@t]
  ?:  .=(d '>')  ?:  (lth .+(x) mapw)  [.+(x) y]  ~
  ?:  .=(d '<')  ?:  (gth x 0)  [(dec x) y]  ~
  ?:  .=(d 'v')  ?:  (lth .+(y) maph)  [x .+(y)]  ~
  ?:  .=(d '^')  ?:  (gth y 0)  [x (dec y)]  ~
~
::  recursively take steps
=/  step
|=  [p=[x=@ud y=@ud] d=@t n=@ud]
=/  np  (npos x.p y.p d)
?^  np  $(p np, n +(n))  n
::::  =/  nx
::::(obst [1 1])
(step spos '^' 0)
