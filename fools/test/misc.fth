require prelude.fth
require misc.fth
require tap.fth

: test-copy-slice ( -- )
  s" abc" copy-slice 2dup s" abc" compare 0= ok
  drop free throw
;

: test-slurp-file ( -- )
  s" test/misc.fth" slurp-file
  drop 11 s" require pre" compare 0= ok
;

: main ( -- )
  test-copy-slice
  test-slurp-file
;

' main 3 run-tests
