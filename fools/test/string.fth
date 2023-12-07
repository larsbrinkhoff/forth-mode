require prelude.fth
require string.fth
require tap.fth

: alpha? ( char -- flag ) 'a' 'z' 1+ within ;

: test1 ( -- )
  ['] alpha? s" 01a34" string-position
  true = ok
  2 = ok

  ['] alpha? s" 01a3b" string-position
  true = ok
  2 = ok

  ['] alpha? s" 01234" string-position
  false = ok
  -1 = ok

  ['] alpha? s" " string-position
  false = ok
  -1 = ok
;

: test2 ( -- )
  ['] alpha? s" 01a34" string-split
  s" a34" compare 0= ok
  s" 01" compare 0= ok

  ['] alpha? s" 01a3b" string-split
  s" a3b" compare 0= ok
  s" 01" compare 0= ok

  ['] alpha? s" 01234" string-split
  s" " compare 0= ok
  s" 01234" compare 0= ok

  ['] alpha? s" a1234" string-split
  s" a1234" compare 0= ok
  s" " compare 0= ok

  ['] alpha? s" " string-split
  s" " compare 0= ok
  s" " compare 0= ok
;

: test-suffix ( -- )
  s" abc" s" abc" string-suffix? ok
  s" abc"  s" ab"  string-suffix? 0= ok
  s" ab" s" abc" string-suffix? 0= ok
  s" " s" abc"  string-suffix? 0= ok
  s" abc" s" " string-suffix? ok
  s" x.c"  s" .c" string-suffix? ok
  s" x.c~" s" .c"  string-suffix? 0= ok
  s" a/" s" /"  string-suffix? ok
;

: test-prefix ( -- )
  s" abc" s" abc" string-prefix? ok
  s" ab" s" abc" string-prefix? ok
  s" abc" s" ab" string-prefix? 0= ok
  s" abc" s" " string-prefix? 0= ok
  s" "  s" abc" string-prefix? ok
  s" .c" s" x.c" string-prefix? 0= ok
  s" .c" s" x.c~" string-prefix? 0= ok
  s" x" s" x.c" string-prefix? ok
  s" x" s" x.c~" string-prefix? ok
  s" /" s" /foo" string-prefix? ok
;

: main ( -- )
  test1
  test2
  test-suffix
  test-prefix
;

' main 37 run-tests
