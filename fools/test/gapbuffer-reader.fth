require prelude.fth
require gapbuffer-reader.fth
require tap.fth

create buf /gapbuffer allot
create mem 100 allot

: test1 ( -- )
  0 buf init-gapbuffer drop
  s" abc" 0 buf gapbuffer-insert
  buf make-gapbuffer-reader {: r :}
  r reader-read-byte throw 'a' = ok
  r reader-read-byte throw 'b' = ok
  r reader-read-byte throw 'c' = ok
  r reader-read-byte throw -1 = ok
  r reader-read-byte throw -1 = ok
;

: space? ( char -- flag ) 'SPACE' = ;
: non-space? ( char -- flag ) 'SPACE' <> ;

: test-skip ( -- )
  0 buf init-gapbuffer drop
  s"     abcd  " 0 buf gapbuffer-insert
  buf make-gapbuffer-reader {: r :}
  r reader-peek-byte throw 'SPACE' = ok
  ['] space? r reader-skip
  r reader-peek-byte throw 'a' = ok
  ['] non-space? r reader-skip
  r reader-peek-byte throw 'SPACE' = ok
  ['] space? r reader-skip
  r reader-peek-byte throw -1 = ok
  ['] non-space? r reader-skip
  r reader-peek-byte throw -1 = ok
;

: main  ( -- )
  test1
  test-skip
;

' main 11 run-tests
