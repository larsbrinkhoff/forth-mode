require prelude.fth
require textdoc.fth
require tap.fth

create doc /textdoc allot
create mem 100 allot

: test-reader ( -- )
  s" file:foo.fth" s" test/textdoc.fth" slurp-file doc init-textdoc drop
  doc make-textdoc-reader >r
  mem 19 r@ reader-read-sloppy throw
  mem 19 s" require prelude.fth" compare 0= ok
  r> drop-textdoc-reader
;

: test-change ( -- )
  s" file:foo.fth" s" abcde" doc init-textdoc drop
  0 1 0 4 s" xyz" doc textdoc-change 0= ok
  mem 100 doc make-textdoc-reader reader-read-sloppy drop
  mem s" axyze" tuck compare 0= ok
;

: main ( -- )
  test-reader
  test-change
;

' main 4 run-tests
