require prelude.fth
require fd-writer.fth
require fd.fth
require tap.fth

create mywriter /fd-writer allot

: test1 ( -- )
  1 mywriter init-fd-writer mywriter = ok
  mywriter deinit-fd-writer

  1 make-fd-writer dup 0<> ok
  drop-fd-writer
;

: test2 ( -- )
  1 mywriter init-fd-writer mywriter = ok
  s\" foo\n" mywriter writer-write throw 4 = ok
  mywriter writer-flush throw
;


create mem 100 allot

: test3 ( -- )
  make-pipe throw		( fd0 fd1 )
  mywriter init-fd-writer mywriter = ok
  s" abc" mywriter writer-write throw 3 = ok
  mywriter writer-flush throw
  mem 100 rot fd-read throw 3 = ok
  mem 3 s" abc" compare 0= ok
;

: main ( -- )
  test1
  test2
  test3
;

' main 9 run-tests
