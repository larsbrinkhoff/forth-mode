
require prelude.fth
require region-writer.fth
require tap.fth

create myregion /region allot
create mywriter /region-writer allot

: test1 ( -- )
  myregion init-region mywriter init-region-writer mywriter = ok
  mywriter region-writer-finish s" " compare 0= ok
;

: test2 ( -- )
  myregion init-region mywriter init-region-writer mywriter = ok
  s" abcef" mywriter writer-write throw 5 = ok
  s" 12345" mywriter writer-write throw 5 = ok
  mywriter region-writer-finish s" abcef12345" compare 0= ok
  myregion region-free-all

  myregion mywriter init-region-writer mywriter = ok
  s" " mywriter writer-write throw 0 = ok
  mywriter region-writer-finish s" " compare 0= ok
  myregion region-free-all
;

: test3 ( -- )
  myregion init-region mywriter init-region-writer mywriter = ok
  s" abcdefghijklmn"
  begin dup 0<> while
    over c@ mywriter writer-write-byte throw
    1 /string
  repeat
  2drop
  mywriter region-writer-finish s" abcdefghijklmn" compare 0= ok
;

: main ( -- )
  test1
  test2
  test3
;

' main 12 run-tests
