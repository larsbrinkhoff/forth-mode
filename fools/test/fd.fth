require prelude.fth
require fd.fth
require tap.fth

9 constant EBADF \ Bad file descriptor

: test-write ( -- )
  s\" abc\n" 1 fd-write throw 4 = ok

  depth {: d :}
  s\" efg\n" -1 ['] fd-write catch
  begin depth d 2 + <> while drop repeat
  EBADF = ok
  -1 = ok
;

: test-pipe ( -- )
  make-pipe throw		{: fd0 fd1 :}

  s" abc" fd1 fd-write throw 3 = ok

  s" xyz" pad swap move
  pad 3 s" xyz" compare 0= ok

  pad 10 fd0 fd-read throw 3 = ok
  pad 3 s" abc" compare 0= ok
;

: main ( -- )
  8 test-plan
  depth >r
  test-write
  test-pipe
  depth r> = ok
;

main
bye
