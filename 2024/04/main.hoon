!:
=/  input=cord
'''
$INPUT
'''
=/  lines  (to-wain:format input)
::  per line we have the match count, sum them
%-  roll  :_  add
=<  p
%^  spin  lines  0
|=  [line=cord i=@ud]
:_  .+  i
::  per line we have the match count per column, sum them
::
%-  roll  :_  add
=<  p
%^  spin  (trip line)  0
|=  [char=@t j=@ud]
:_  .+  j
^-  @ud
::  extract the 3 base cases of substring from the grid
::
=/  horz
  %^  cut  3  [j 4]  line
=/  vert
  ?.  (lth (add 3 i) (lent lines))  '    '
  %-  crip
  :~
    char
    (cut 3 [j 1] (snag (add 1 i) lines))
    (cut 3 [j 1] (snag (add 2 i) lines))
    (cut 3 [j 1] (snag (add 3 i) lines))
  ==
=/  nwse  :: north-west south-east
  ?.  ?&  (lth (add 3 i) (lent lines))
          (lth (add 3 j) (met 3 line))
      ==
    '    '
  %-  crip
  :~
    char
    (cut 3 [(add 1 j) 1] (snag (add 1 i) lines))
    (cut 3 [(add 2 j) 1] (snag (add 2 i) lines))
    (cut 3 [(add 3 j) 1] (snag (add 3 i) lines))
  ==
=/  nesw  :: north-east south-west
  ?.  ?&  (lth (add 3 i) (lent lines))
          (lth (add 3 j) (met 3 line))
      ==
    '    '
  %-  crip
  :~
    (cut 3 [(add 3 j) 1] (snag (add 0 i) lines))
    (cut 3 [(add 2 j) 1] (snag (add 1 i) lines))
    (cut 3 [(add 1 j) 1] (snag (add 2 i) lines))
    (cut 3 [(add 0 j) 1] (snag (add 3 i) lines))
  ==
::  for the six cases, check for a match, then sum the match counts
::  we need to negate them because true is 0 and false is 1
::
%-  roll  :_  add
^-  (list @)
:~
  !=(horz 'XMAS')
  !=(vert 'XMAS')
  !=(nwse 'XMAS')
  !=(nesw 'XMAS')
  !=((rev 3 4 horz) 'XMAS')
  !=((rev 3 4 vert) 'XMAS')
  !=((rev 3 4 nwse) 'XMAS')
  !=((rev 3 4 nesw) 'XMAS')
 ==
