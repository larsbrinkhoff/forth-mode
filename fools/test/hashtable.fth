require prelude.fth
require hashtable.fth
require tap.fth

create my-ht /hashtable allot

create my-mem 1000 cells allot

0 value my-end

: my-alloc ( u env -- a-addr ior )
  drop >r
  my-mem my-end +
  r> my-end + to my-end
  0
;

: my-free ( a-addr env -- ior ) 0 ;
: my-resize ( a-addr env -- ior ) true abort" nyi" ;

create my-allocator-vtable
' my-alloc ' my-free ' my-resize
here /allocator-vtable allot init-allocator-vtable
0 swap 2constant my-allocator


: id-hash ( x -- x ) ;

: test-init ( -- )
  ['] id-hash ['] = 0 my-allocator my-ht init-hashtable drop
;

: test-put ( -- )
  ['] id-hash ['] = make-hashtable {: ht :}
  123 456 ht hashtable-put
  123 ht hashtable-get ok 456 = ok
  ht hashtable-count 1 = ok

  25 0 ?do
    i i negate ht hashtable-put
  loop
  25 0 ?do
    i ht hashtable-get ok i negate = ok
  loop
  ht hashtable-count 26 = ok

  ht drop-hashtable
;

: %foo ( i key value -- i+1 ) , , 1+ ;

: test-for-each ( -- )
  ['] id-hash ['] = make-hashtable {: ht :}

  25 0 ?do
    i i invert ht hashtable-put
  loop

  here
  0 ['] %foo ht hashtable-for-each
  25 = ok
  here over - 50 cells = ok
  drop
;

: main ( -- )
  test-init
  test-put
  test-for-each
;

' main 57 run-tests
