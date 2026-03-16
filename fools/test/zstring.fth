require prelude.fth
require zstring.fth
require tap.fth

: test-strlen ( -- )
  s\" abc\z" drop strlen 3 = ok
  s\" \z" drop strlen 0 = ok
;

: %f1 ( zstring -- )
  dup strlen s" abc" compare 0= ok
  123
;

: %f2 ( zstring -- )
  dup strlen s" " compare 0= ok
  456
;

: test-with-zstring  ( -- )
  ['] %f1 s" abc" with-zstring
  123 = ok
  ['] %f2 s" " with-zstring
  456 = ok
;

: main ( -- )
  test-strlen
  test-with-zstring
;

' main 7 run-tests
