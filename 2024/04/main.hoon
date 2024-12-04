!:
=/  input=cord
  '''
  MMMSXXMASM
  MSAMXMSMSA
  AMXSXMAAMM
  MSAMASMSMX
  XMASAMXAMM
  XXAMMXXAMA
  SMSMSASXSS
  SAXAMASAAA
  MAMMMXMMMM
  MXMXAXMASX
  '''
=/  lines
  %-  to-wain:format  input
%^  spin  lines  0
|=  [line=cord i=@ud]
:_  .+  i
%^  spin  (trip line)  0
|=  [char=@t j=@ud]
:_  .+  j
^-  @t
=/  hor
  %^  cut  3  [j 4]  line
=/  ver
  ?.  (lth (add 3 i) (lent lines))  '    '
  %-  crip
  :~
    char
    (cut 3 [j 1] (snag (add 1 i) lines))
    (cut 3 [j 1] (snag (add 2 i) lines))
    (cut 3 [j 1] (snag (add 3 i) lines))
  ==
=/  diag
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
diag
