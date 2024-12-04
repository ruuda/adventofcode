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
::  extract the 2 diagonals from the grid
::
=/  nwse  :: north-west south-east
  ?.  ?&  (lth (add 2 i) (lent lines))
          (lth (add 2 j) (met 3 line))
      ==
    '    '
  %-  crip
  :~
    char
    (cut 3 [(add 1 j) 1] (snag (add 1 i) lines))
    (cut 3 [(add 2 j) 1] (snag (add 2 i) lines))
  ==
=/  nesw  :: north-east south-west
  ?.  ?&  (lth (add 2 i) (lent lines))
          (lth (add 2 j) (met 3 line))
      ==
    '    '
  %-  crip
  :~
    (cut 3 [(add 2 j) 1] (snag (add 0 i) lines))
    (cut 3 [(add 1 j) 1] (snag (add 1 i) lines))
    (cut 3 [(add 0 j) 1] (snag (add 2 i) lines))
  ==
::  check for the four possible X-MASes
::  negate because we want 1 on match and false is 1
::
^-  @  ?!
?|
  &(=(nwse 'MAS') =(nesw 'MAS'))
  &(=(nwse 'MAS') =(nesw 'SAM'))
  &(=(nwse 'SAM') =(nesw 'MAS'))
  &(=(nwse 'SAM') =(nesw 'SAM'))
==
