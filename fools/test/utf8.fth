require prelude.fth
require utf8.fth
require tap.fth

: test-size ( -- )
  $24 utf8-encoded-size 1 = ok
  $A3 utf8-encoded-size 2 = ok
  $0939 utf8-encoded-size 3 = ok
  $20AC utf8-encoded-size 3 = ok
  $D55C utf8-encoded-size 3 = ok
  $1D4AC utf8-encoded-size 4 = ok
;

variable var

: check-encode ( codepoint expected$ -- )
  2>r
  0 var !
  dup var utf8-encode
  utf8-encoded-size
  var swap 2r> compare 0= ok
;

: test-encode ( -- )
  $24 s" $" check-encode
  $A3 s" £" check-encode
  $20AC s" €" check-encode
  $1D4AC s" 𝒬" check-encode
;

: main  ( -- )
  test-size
  test-encode
;

' main 11 run-tests
