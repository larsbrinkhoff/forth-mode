require prelude.fth
require fd-reader.fth
require fd.fth
require tap.fth

: test1 ( -- )
  make-pipe throw swap make-fd-reader {: fd1 reader :}
  reader 0<> ok

  s" abc" fd1 fd-write throw
  3 = ok
  reader reader-read-byte throw 'a' = ok
  reader reader-read-byte throw 'b' = ok
  reader reader-read-byte throw 'c' = ok

  s\" de\r\n" fd1 fd-write throw
  4 = ok
  pad 10 reader reader-read-line-crlf throw
  false = ok
  2 = ok
  pad 2 s" de" compare 0= ok

  s\" \r\n" fd1 fd-write throw
  2 = ok
  pad 10 reader reader-read-line-crlf throw
  false = ok
  0 = ok
  pad 0 s" " compare 0= ok

  s\" abc\nefg\r\n" fd1 fd-write throw
  9 = ok
  pad 10 reader reader-read-line-crlf throw
  false = ok
  7 = ok
  pad 7 s\" abc\nefg" compare 0= ok


  s\" xy\r\r\n" fd1 fd-write throw
  5 = ok
  pad 10 reader reader-read-line-crlf throw
  false = ok
  3 = ok
  pad 3 s\" xy\r" compare 0= ok

  s" abcdefghijklmnopqrstuvwxyz" fd1 fd-write throw
  26 = ok
  pad 26 reader reader-read-sloppy throw
  pad 26 s" abcdefghijklmnopqrstuvwxyz" compare 0= ok

  s" 1234" fd1 fd-write throw
  4 = ok
  reader reader-read-byte throw
  '1' = ok
  reader reader-read-byte throw
  '2' = ok
  reader reader-peek-byte throw
  '3' = ok
  reader reader-read-byte throw
  '3' = ok
  reader reader-read-byte throw
  '4' = ok
;

: main ( -- )
  test1
;

' main 30 run-tests
