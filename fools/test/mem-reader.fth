
require prelude.fth
require mem-reader.fth
require tap.fth

create mreader /mem-reader allot

: test1 ( -- )
  s" abc" mreader init-mem-reader drop

  mreader reader-read-byte throw 'a' = ok
  mreader reader-read-byte throw 'b' = ok
  mreader reader-read-byte throw 'c' = ok
  mreader reader-read-byte throw -1 = ok
  mreader reader-read-byte throw -1 = ok
;

: test2 ( -- )
  s" 123" make-mem-reader {: r :}

  r reader-read-byte throw '1' = ok
  r reader-read-byte throw '2' = ok
  r reader-read-byte throw '3' = ok
  r reader-read-byte throw -1 = ok
  r reader-read-byte throw -1 = ok

  r drop-mem-reader
;

: test3 ( -- )
  s" " mreader init-mem-reader drop
  mreader reader-peek-byte 0= ok -1 = ok

  s" ab" 2dup mreader init-mem-reader drop
  mreader reader-peek-byte 0= ok 'a' = ok
  mreader reader-read-byte 0= ok 'a' = ok
  mreader reader-peek-byte 0= ok 'b' = ok
  mreader reader-read-byte 0= ok 'b' = ok
  mreader reader-peek-byte 0= ok -1 = ok
  mreader reader-read-byte 0= ok -1 = ok
  s" ab" compare 0= ok
;

: space? ( char -- flag ) 'SPACE' = ;
: non-space? ( char -- flag ) 'SPACE' <> ;

: test-skip ( -- )
  s"     abcd  " mreader init-mem-reader drop
  mreader reader-peek-byte throw 'SPACE' = ok
  ['] space? mreader reader-skip
  mreader reader-peek-byte throw 'a' = ok
  ['] non-space? mreader reader-skip
  mreader reader-peek-byte throw 'SPACE' = ok
  ['] space? mreader reader-skip
  mreader reader-peek-byte throw -1 = ok
  ['] space? mreader reader-skip
  mreader reader-peek-byte throw -1 = ok
;

: test-looking-at ( -- )
  s" abc" mreader init-mem-reader drop
  s" abc" mreader %reader-looking-at? 0= ok 0= ok 0= ok
  mreader %reader-buffer buffer-slice s" abc" compare 0= ok
  s" abcd" mreader %reader-looking-at? 0= ok ok 1 = ok
  mreader %reader-buffer buffer-slice s" abc" compare 0= ok
  s" abcd" mreader %reader-looking-at? 0= ok ok 1 = ok
  s" " mreader %reader-looking-at? 0= ok 0= ok 0 = ok

  s" a" mreader init-mem-reader drop
  s" abcd" mreader %reader-looking-at? 0= ok ok 3 = ok

  s" " mreader init-mem-reader drop
  s" " mreader %reader-looking-at? 0= ok 0= ok 0 = ok
  s" abc" mreader %reader-looking-at? 0= ok ok 3 = ok

  \ The string should not be modified.
  s" abcdefghi" 2dup mreader init-mem-reader drop
  here 8 mreader reader-read-sloppy 0= ok
  s" ijk" mreader %reader-looking-at? 0= ok ok 2 = ok
  s" abcdefghi" compare 0= ok
;

create line 1024 allot

: test-read-line ( -- )
  s" " mreader init-mem-reader drop
  line 1024 mreader reader-read-line-crlf 0= ok ok 0= ok

  s" abc" mreader init-mem-reader drop
  line 1024 mreader reader-read-line-crlf 0= ok ok 3 = ok
  line 3 s" abc" compare 0= ok

  s\" \r\n" mreader init-mem-reader drop
  line 1024 mreader reader-read-line-crlf 0= ok 0= ok 0 = ok
  line 0 s" " compare 0= ok
  line 1024 mreader reader-read-line-crlf 0= ok ok 0 = ok
  line 0 s" " compare 0= ok

  s\" abc\r\n" mreader init-mem-reader drop
  line 1024 mreader reader-read-line-crlf 0= ok 0= ok 3 = ok
  line 3 s" abc" compare 0= ok
  line 1024 mreader reader-read-line-crlf 0= ok ok 0 = ok
  line 0 s" " compare 0= ok

  s\" \r" mreader init-mem-reader drop
  line 1024 mreader reader-read-line-crlf 0= ok ok 1 = ok
  line 1 s\" \r" compare 0= ok
  line 1024 mreader reader-read-line-crlf 0= ok ok 0 = ok
  line 0 s" " compare 0= ok

  s\" \r" mreader init-mem-reader drop
  line 0 mreader reader-read-line-crlf 0= ok 0= ok 0 = ok

;

: main ( -- )
  test1
  test2
  test3
  test-skip
  test-looking-at
  test-read-line
;

' main 93 run-tests
