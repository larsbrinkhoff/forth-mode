require prelude.fth
require file-reader.fth
require tap.fth

create mem 100 allot

: test1 ( -- )
  s" test/file-reader.fth" make-file-reader {: r :}
  mem 100 r reader-read-sloppy throw
  mem s" require prelude.fth" tuck compare 0= ok
  r drop-file-reader
;

: main ( -- )
  test1
;

' main 2 run-tests
