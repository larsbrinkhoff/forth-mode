require prelude.fth
require json.fth
require tap.fth

: test-fixnum ( -- )
  3456 json-make-fixnum json-integer-value 3456 = ok
  -789 json-make-fixnum json-integer-value -789 = ok
;

: test-true ( -- )
  json-true json-true? ok
  json-false json-true? 0= ok
  json-null json-true? 0= ok
  123 json-make-fixnum json-true? 0= ok
;

: test-false ( -- )
  json-false json-false? ok
  json-true json-false? 0= ok
  json-null json-false? 0= ok
  123 json-make-fixnum json-false? 0= ok
;

: test-null ( -- )
  json-null json-null? ok
  json-true json-null? 0= ok
  json-false json-null? 0= ok
  123 json-make-fixnum json-null? 0= ok
;

create myregion /region allot

: test-make-string ( -- )
  myregion init-region drop
  s" abc" myregion json-make-string
  dup json-string? ok
  json-string-slice s" abc" compare 0= ok

  s" " myregion json-make-string
  dup json-string? ok
  json-string-slice s" " compare 0= ok
;

create myvals json-true , json-false , json-null ,

: test-array ( -- )
  myregion init-region drop
  myvals 3 myregion json-make-array
  dup json-array? ok
  dup json-array-length 3 = ok
  0 over json-array-get myvals 0 cells + @ = ok
  1 over json-array-get myvals 1 cells + @ = ok
  2 over json-array-get myvals 2 cells + @ = ok
  drop
;

: main  ( -- )
  test-fixnum
  test-true
  test-false
  test-null
  test-make-string
  test-array
;


' main 24 run-tests
