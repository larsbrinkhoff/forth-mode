require prelude.fth
require assert.fth
require tap.fth

: test1 ( -- )
  assert( false )
  false ok
;

: test2 ( -- )
  assert( true )
  true ok
;

: main  ( -- )
  ['] test1 catch -2 = ok
  ['] test2 catch 0 = ok
;

' main 4 run-tests
