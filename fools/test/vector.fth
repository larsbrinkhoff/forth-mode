
require prelude.fth
require vector.fth
require tap.fth

create mvec /vector allot

: test-init ( -- )
  mvec init-vector mvec = ok
  mvec vector-length 0= ok
;

: test-push-slice ( -- )
  mvec init-vector drop
  s" abc" mvec vector-push-slice
  mvec vector-slice s" abc" compare 0= ok
  here 100 cells mvec vector-push-slice
  mvec vector-length 100 cells 3 + = ok
;

: test-push-cell ( -- )
  mvec init-vector drop
  1 mvec vector-push-cell
  2 mvec vector-push-cell
  mvec vector-slice 2 cells = ok
  dup @ 1 = ok
  cell+ @ 2 = ok
  mvec vector-length 2 cells = ok
;

: odd? ( a-addr -- flag ) @ 2 mod 0= ;

: test-filter ( -- )
  mvec init-vector drop
  10 0 do
    i mvec vector-push-cell
  loop
  ['] odd? 1 cells mvec vector-filter {: v :}
  v vector-length 5 cells = ok
  5 0 do
    v vector-base i cells + @ i 2* = ok
  loop
  v drop-vector
;

: %f1 ( c-addr -- flag ) c@ '0' - 2 mod 0<> ;
: %f2 ( c-addr -- flag ) %f1 0= ;
: %f3 ( c-addr -- flag ) c@ '0' - 3 mod 0<> ;
: %f4 ( c-addr -- flag ) %f3 0= ;
: %f5 ( c-addr -- flag ) c@ '0' - 4 mod 0<> ;
: %f6 ( c-addr -- flag ) %f5 0= ;

: test-delete ( -- )
  mvec init-vector drop
  s" 0123456789" mvec vector-push-slice
  ['] %f1 1 mvec vector-delete
  mvec vector-slice s" 02468" compare 0= ok

  mvec init-vector drop
  s" 0123456789" mvec vector-push-slice
  ['] %f2 1 mvec vector-delete
  mvec vector-slice s" 13579" compare 0= ok

  mvec init-vector drop
  s" 0123456789" mvec vector-push-slice
  ['] %f3 1 mvec vector-delete
  mvec vector-slice s" 0369" compare 0= ok

  mvec init-vector drop
  s" 0123456789" mvec vector-push-slice
  ['] %f4 1 mvec vector-delete
  mvec vector-slice s" 124578" compare 0= ok

  mvec init-vector drop
  s" 0123456789" mvec vector-push-slice
  ['] %f5 1 mvec vector-delete
  mvec vector-slice s" 048"  compare 0= ok

  mvec init-vector drop
  s" 0123456789" mvec vector-push-slice
  ['] %f6 1 mvec vector-delete
  mvec vector-slice s" 1235679"  compare 0= ok
;

: main ( -- )
  test-init
  test-push-slice
  test-push-cell
  test-filter
  test-delete
;

' main 21 run-tests
