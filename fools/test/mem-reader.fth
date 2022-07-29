
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
  mreader reader-peek-byte throw -1 = ok
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

: main ( -- )
  test1
  test2
  test3
  test-skip
;

' main 17 run-tests
