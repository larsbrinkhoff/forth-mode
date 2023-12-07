require prelude.fth
require json-builder.fth
require tap.fth

create myregion /region allot
create jb /jb allot

: test-init ( -- )
  myregion init-region jb init-jb jb = ok
  jb deinit-jb
;

: test-string ( -- )
  myregion init-region jb init-jb drop
  jb jb-begin-string
  'a' jb jb-add-byte
  'b' jb jb-add-byte
  'c' jb jb-add-byte
  jb jb-finish-string json-string-slice s" abc" compare 0= ok
;

: test-array ( -- )
  myregion init-region jb init-jb drop
  jb jb-begin-array
  jb jb-finish-array json-array-length 0= ok

  jb jb-begin-array
  json-true jb jb-push
  json-false jb jb-push
  jb jb-finish-array
  dup json-array-length 2 = ok
  0 over json-array-get json-true = ok
  1 over json-array-get json-false = ok
  drop
;

: test-object ( -- )
  myregion init-region jb init-jb drop
  jb jb-begin-object
  jb jb-finish-object json-object-count 0= ok

  jb jb-begin-object
  s" k" myregion json-make-string json-false jb jb-push-member
  jb jb-finish-object
  dup json-object-count 1 = ok
  s" k" rot json-object-get ok json-false = ok
;

: main  ( -- )
  test-init
  test-string
  test-array
  test-object
;

' main 11 run-tests
