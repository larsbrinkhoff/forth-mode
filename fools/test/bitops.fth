require prelude.fth
require bitops.fth
require tap.fth

: test-align ( -- )
  0 cell-aligned? ok
  1 cell-aligned? 0= ok
  -1 cell-aligned? 0= ok
  1 cells cell-aligned? ok
  1 cells 1+ cell-aligned? 0= ok
  1 cells 1- cell-aligned? 0= ok
  0 cell-aligned 0 = ok
  1 cell-aligned 1 cells = ok
  1 cells cell-aligned 1 cells = ok
  1 cells 1- cell-aligned 1 cells = ok
  1 cells 1+ cell-aligned 2 cells = ok
;


: main  ( -- )
  test-align
;

' main 12 run-tests
