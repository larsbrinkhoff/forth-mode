\ tap.fth --- Utils for writing tests
\
\ Helpers for writing tests according to the Test Anything Protocol.
\ See http://testanything.org/.

1 value %tap-counter

: %tap-next-number ( -- u ) %tap-counter dup 1+ to %tap-counter ;
: %with-base ( xt u -- ) base @ >r base ! catch r> base ! throw ;
: %.num ( u -- ) ['] u. 10 %with-base ;

: test-plan ( u -- ) cr ." 1.." %.num cr ;
: ok ( flag -- ) 0= if ." not " then ." ok " %tap-next-number %.num cr ;

: run-tests ( xt u )
  test-plan
  depth 1- >r
  execute
  depth r> = ok
  bye
;
