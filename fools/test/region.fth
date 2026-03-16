
require prelude.fth
require region.fth
require tap.fth

create myregion /region allot

0 value my-used
0 value my-last-freed
create my-chunk 1000 cells allot
create my-pad 2000 cells allot

: my-malloc ( u env -- a-addr ior )
  my-chunk = ok
  my-chunk my-used +
  swap my-used + to my-used
  0
;

: my-malloc100 ( u env -- a-addr ior )
  over 100 cells = ok
  my-malloc
;

: my-free ( u env -- a-addr 0 )
  my-chunk = ok
  dup my-chunk my-chunk my-chunk + within ok
  to my-last-freed
  0
;

: my-resize ( a-addr1 u env -- a-addr2 ior ) true abort" nyi" ;

create my-allocator-vtable
' my-malloc ' my-free ' my-resize
here /allocator-vtable allot init-allocator-vtable drop

create my-100allocator-vtable
' my-malloc100 ' my-free ' my-resize
here /allocator-vtable allot init-allocator-vtable drop

: my-allocator    ( -- allocator ) my-chunk my-allocator-vtable ;
: my-100allocator ( -- allocator ) my-chunk my-100allocator-vtable ;

: test-init ( -- )
  myregion init-region myregion = ok
  myregion region-object-size 0= ok

  100 cells my-100allocator myregion init-region-with-args myregion = ok
  myregion %region-base @ my-chunk %/chunk-header + = ok
  myregion %region-free @ my-chunk %/chunk-header + = ok
  myregion region-object-size 0= ok
;

: test-deinit ( -- )
  100 cells my-100allocator myregion init-region-with-args myregion = ok
  myregion deinit-region
;

: test-alloc ( -- )
  myregion init-region myregion = ok
  3 myregion region-alloc
  dup 0<> ok
  dup cell-aligned? ok
  drop

  7 myregion region-alloc
  dup 0<> ok
  dup cell-aligned? ok
  drop

  0 to my-used
  100 cells my-allocator myregion init-region-with-args myregion = ok
  myregion %region-base @ my-chunk %/chunk-header + = ok

  10 cells myregion region-alloc
  my-chunk %/chunk-header + = ok

  40 cells myregion region-alloc
  my-chunk %/chunk-header + 10 cells + = ok

  60 cells myregion region-alloc
  dup 0<> ok
  dup cell-aligned? ok
  my-chunk 100 cells + %/chunk-header + = ok
  myregion %region-chunk @ my-chunk 100 cells + = ok

  20 cells myregion region-alloc
  dup 0<> ok
  dup cell-aligned? ok
  my-chunk 100 cells + %/chunk-header + 60 cells + = ok

  0 to my-used
  0 to my-last-freed
  100 cells my-allocator myregion init-region-with-args myregion = ok

  110 cells myregion region-alloc
  my-chunk 100 cells + %/chunk-header + = ok
  my-last-freed my-chunk = ok
  myregion %region-chunk @ my-chunk 100 cells + = ok

  0 cells myregion region-alloc 0<> ok
;

: test-add-byte ( -- )
  myregion init-region myregion = ok
  1 cells myregion region-blank
  'a' myregion region-add-byte
  myregion region-object-size 1 cells 1+ = ok
  'b' myregion region-add-byte
  myregion region-object-size 1 cells 2 + = ok
;

: test-add-slice ( -- )
  myregion init-region myregion = ok
  s" abc" myregion region-add-slice
  s" 12345678" myregion region-add-slice
  'b' myregion region-add-byte
  myregion region-object-size 12 = ok
  myregion region-finish 12 s" abc12345678b" compare 0= ok
;


: test-free ( -- )
  0 {: addr1 :}
  0 to my-used
  0 to my-last-freed
  100 cells my-allocator myregion init-region-with-args myregion = ok

  1 cells myregion region-alloc to addr1
  addr1 my-chunk %/chunk-header + = ok

  150 cells myregion region-alloc
  my-chunk 100 cells + %/chunk-header + = ok
  my-last-freed 0= ok
  myregion %region-chunk @ my-chunk 100 cells + = ok

  addr1 myregion region-free
  1 cells myregion region-alloc
  addr1 = ok

  0 to my-used
  0 to my-last-freed
  0 to addr1
  100 cells my-allocator myregion init-region-with-args myregion = ok

  0 cells myregion region-alloc to addr1
  addr1 my-chunk %/chunk-header + cell+ = ok

  150 cells myregion region-alloc
  my-chunk 100 cells + %/chunk-header + = ok
  my-last-freed 0= ok
  myregion %region-chunk @ my-chunk 100 cells + = ok

  addr1 myregion region-free
  my-last-freed my-chunk 100 cells + = ok

  0 cells myregion region-alloc
  my-chunk %/chunk-header + cell+ = ok
  0 cells myregion region-alloc
  my-chunk %/chunk-header + cell+ = ok

  my-last-freed my-chunk 100 cells + = ok
  0 to my-last-freed

  150 cells myregion region-alloc drop
  my-last-freed 0= ok
;

: test-free-all ( -- )
  0 to my-used
  0 to my-last-freed
  100 cells my-allocator myregion init-region-with-args myregion = ok

  100 cells %/chunk-header - myregion region-alloc
  my-chunk %/chunk-header + = ok
  myregion region-next-free my-chunk %chunk-limit = ok

  100 cells %/chunk-header - myregion region-alloc
  my-chunk 100 cells + %/chunk-header + = ok

  100 cells %/chunk-header - myregion region-alloc drop
  myregion region-free-all
  my-last-freed my-chunk 100 cells + =  ok
  myregion region-object-size 0= ok
  myregion region-next-free my-chunk %/chunk-header + = ok
  myregion region-room 100 cells %/chunk-header - = ok
;

: main ( -- )
  test-init
  test-deinit
  test-alloc
  test-add-byte
  test-add-slice
  test-free
  test-free-all
;

' main 87 run-tests
