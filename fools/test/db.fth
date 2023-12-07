
require prelude.fth
require db.fth
require tap.fth

: test-init ( -- )
  make-db 0<> ok
;

: test-intern ( -- )
  make-db 0 {: db uri :}
  s" file:/foo/bar"
  2dup db %db-intern-string to uri
  2dup uri %db-string-slice compare 0= ok
  2dup db %db-intern-string uri = ok
  2drop
;

: test-insert-def ( -- )
  make-db {: db | v :}
  s" foo" s" file:/bar/baz" 10 0 s" " db db-insert-definition

  s" foo" db db-select-definitions to v
  v vector-length /definition = ok
  v vector-base
  %definition-uri @ %db-string-slice s" file:/bar/baz" compare 0= ok
  v drop-vector
;

: test-delete-def ( -- )
  make-db {: db | v :}
  s" foo" s" file:/bar/baz" 10 0 s" " db db-insert-definition
  s" foo" s" file:/bar/quux" 10 0 s" " db db-insert-definition
  s" bar" s" file:/bar/baz" 12 0 s" " db db-insert-definition

  s" foo" db db-select-definitions to v
  v vector-length 2 /definition * = ok

  s" file:/bar/baz" db db-delete-definitions-with-uri

  s" foo" db db-select-definitions to v
  v vector-length 1 /definition * = ok
  s" bar" db db-select-definitions to v
  v vector-length 0= ok
;

: main ( -- )
  test-init
  test-intern
  test-insert-def
  test-delete-def
;

' main 9 run-tests
