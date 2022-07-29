
require prelude.fth
require terminal-writer.fth
require tap.fth

: test1 ( -- )
  s\" abcd\n" terminal-writer writer-write 0= ok 5 = ok
  terminal-writer writer-flush 0= ok
;

: main ( -- )
  test1
;

' main 4 run-tests
