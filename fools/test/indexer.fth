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

: test-definitions ( -- )
  s" file:foo.fth"
  s" 1 constant const1 0 value val1 0 0 +field field1 variable var1"
  doc init-textdoc drop
  make-db {: db :}
  db indexer init-indexer drop
  doc indexer indexer-process-textdoc
  s" const1" db db-select-definitions vector-length /definition = ok
  s" val1" db db-select-definitions vector-length /definition = ok
  s" field1" db db-select-definitions vector-length /definition = ok
  s" var1" db db-select-definitions vector-length /definition = ok

  s" file:foo.fth"
  s" begin-structure struct1 end-structure synonym syn1 foo buffer: buf1"
  doc init-textdoc drop
  make-db to db
  db indexer init-indexer drop
  doc indexer indexer-process-textdoc
  s" struct1" db db-select-definitions vector-length /definition = ok
  s" syn1" db db-select-definitions vector-length /definition = ok
  s" buf1" db db-select-definitions vector-length /definition = ok

  s" file:foo.fth"
  s" defer def1 ' + is def1 1 1 2constant 2const1 0 0 2value 2val1"
  doc init-textdoc drop
  make-db to db
  db indexer init-indexer drop
  doc indexer indexer-process-textdoc
  s" def1" db db-select-definitions vector-length /definition 2* = ok
  s" 2const1" db db-select-definitions vector-length /definition = ok
  s" 2val1" db db-select-definitions vector-length /definition = ok

  s" file:foo.fth"
  s" field: field1 cfield: cfield1 code code1 mov 1,r1"
  doc init-textdoc drop
  make-db to db
  db indexer init-indexer drop
  doc indexer indexer-process-textdoc
  s" field1" db db-select-definitions vector-length /definition = ok
  s" cfield1" db db-select-definitions vector-length /definition = ok
  s" code1" db db-select-definitions vector-length /definition = ok

  s" file:foo.fth"
  s\" \\ : f1 1 ; \n: bar ; ( : f2 2 ; ) s\" : f3 3 ; \""
  doc init-textdoc drop
  make-db to db
  db indexer init-indexer drop
  doc indexer indexer-process-textdoc
  s" bar" db db-select-definitions vector-length /definition = ok
  s" f1" db db-select-definitions vector-length 0 = ok
  s" f2" db db-select-definitions vector-length 0 = ok
  s" f3" db db-select-definitions vector-length 0 = ok

;

: main ( -- )
  test-init
  test-process-file
  test-process-directory
  test-process-textdoc
  test-process-file-and-textdoc
  test-definitions
;

' main 29 run-tests
