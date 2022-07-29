require prelude.fth
require indexer.fth
require tap.fth

create indexer /indexer allot

get-current wordlist set-current
: foo 1 ;
set-current
: foo 2 ;

: test-init ( -- )
  make-db indexer init-indexer drop
;

: test-process-file ( -- )
  make-db {: db :}
  db indexer init-indexer drop
  s" test/indexer.fth" indexer indexer-process-file
  \ db db-list-definitions
  s" foo" db db-select-definitions
  dup vector-length 2 /definition * = ok
  dup vector-base %definition-line @ 7 = ok
  dup vector-base %definition-column @ 5 = ok
  drop-vector
;

: test-process-directory ( -- )
  make-db {: db :}
  db indexer init-indexer drop
  s" ./" indexer indexer-process-directory
  s" test-process-directory" db db-select-definitions
  \ db db-list-definitions
  dup vector-length 1 /definition * = ok
  dup vector-base %definition-line @ 27 = ok
  dup vector-base %definition-column @ 24 = ok
  drop-vector
;

create doc /textdoc allot

: test-process-textdoc ( -- )
  s" file:foo.fth" s" : foo ; : bar ;" doc init-textdoc drop
  make-db {: db :}
  db indexer init-indexer drop
  doc indexer indexer-process-textdoc
  \ db db-list-definitions
  s" bar" db db-select-definitions
  dup vector-length 1 /definition * = ok
  dup vector-base %definition-line @ 0 = ok
  dup vector-base %definition-column @ 13 = ok
  drop-vector
;

: test-process-file-and-textdoc ( -- )
  make-db {: db :}
  db indexer init-indexer drop
  s" test/indexer.fth" indexer indexer-process-file

  s" foo" db db-select-definitions
  vector-length 2 /definition * = ok

  s" file:test/indexer.fth" s" : foo ;" doc init-textdoc drop
  doc indexer indexer-process-textdoc

  \ db db-list-definitions

  s" foo" db db-select-definitions
  vector-length 1 /definition * = ok

;

: main ( -- )
  test-init
  test-process-file
  test-process-directory
  test-process-textdoc
  test-process-file-and-textdoc
;

' main 12 run-tests
