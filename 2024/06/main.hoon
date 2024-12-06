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
=/  obst  |=  [p=[x=@ud y=@ud]]  =('#' (snag x.p (trip (snag y.p lines))))
::  check how big the map is
=/  mapw  (met 3 (snag 0 lines))
=/  maph  (lent lines)
::  find next position, return ~ or the position itself
=/  npos
|=  [x=@ud y=@ud d=@t]
  ?:  .=(d '>')  ?:  (lth .+(x) mapw)  [.+(x) y]  ~
  ?:  .=(d '<')  ?:  (gth x 0)  [(dec x) y]  ~
  ?:  .=(d 'v')  ?:  (lth .+(y) maph)  [x .+(y)]  ~
  ?:  .=(d '^')  ?:  (gth y 0)  [x (dec y)]  ~
~
::  rotate right 90 degrees
=/  rotr
|=  d=@t
  ?:  .=(d '>')  'v'
  ?:  .=(d 'v')  '<'
  ?:  .=(d '<')  '^'
  ?:  .=(d '^')  '>'
  '?'
::  recursively take steps
=/  step
|=  [p=[x=@ud y=@ud] d=@t n=@ud]
=/  np  (npos x.p y.p d)
?^  np
  ::  if there is a next position, is it obstructed?
  ?:  (obst np)  $(d (rotr d))  $(p np, n +(n))
::  there is no next position, we are done
n
::  kick off the recursion at the start position
(step spos '^' 0)
