require prelude.fth
require reader.fth
require tap.fth

\ A reader that reads bytes from the region SRC/SRC-LEN.  When the end
\ is reached return IOR.
begin-structure /dbg-reader
  /reader +
  field: dbg-reader-src
  field: dbg-reader-src-pos
  field: dbg-reader-src-len
  field: dbg-reader-ior
  field: dbg-reader-buf
end-structure

: dbg-read ( c-addr u dbg-reader -- u2 ior )
  {: a u r :}
  u 0= if 0 0 exit then
  r dbg-reader-src-pos @ r dbg-reader-src-len @ = if
    0 r dbg-reader-ior @
  else
    r dbg-reader-src @ r dbg-reader-src-pos @ + c@ a c!
    1 r dbg-reader-src-pos +!
    1 0
  then
;

: init-dbg-reader ( src src-len ior a-addr -- reader )
  >r
  r@ ['] dbg-read r@ dbg-reader-buf 1 cells 0 r@ init-reader-with-buffer drop
  r@ dbg-reader-ior !
  r@ dbg-reader-src-len !
  r@ dbg-reader-src !
  0 r@ dbg-reader-src-pos !
  assert( r@ %reader-read-xt @ ['] dbg-read = )
  assert( r@ %reader-env @ r@ = )
  r>
;

create myreader /dbg-reader allot

: test-read-byte ( -- )
  s" abc" 0 myreader init-dbg-reader drop

  myreader reader-read-byte throw 'a' = ok
  myreader reader-read-byte throw 'b' = ok
  myreader reader-read-byte throw 'c' = ok
  myreader reader-read-byte throw -1 = ok
  myreader reader-read-byte throw -1 = ok

  s" xyz" 42 myreader init-dbg-reader drop
  myreader reader-read-byte 0= ok 'x' = ok
  myreader reader-read-byte 0= ok 'y' = ok
  myreader reader-read-byte 0= ok 'z' = ok
  myreader reader-read-byte 42 = ok 0 = ok
  myreader reader-read-byte 42 = ok 0 = ok
;

: test-peek-byte ( -- )
  s" " 0 myreader init-dbg-reader drop
  myreader reader-peek-byte 0= ok -1 = ok

  s" " 42 myreader init-dbg-reader drop
  myreader reader-peek-byte 42 = ok  0= ok

  s" ab" 2dup 0 myreader init-dbg-reader drop
  myreader reader-peek-byte 0= ok 'a' = ok
  myreader reader-read-byte 0= ok 'a' = ok
  myreader reader-peek-byte 0= ok 'b' = ok
  myreader reader-read-byte 0= ok 'b' = ok
  myreader reader-peek-byte 0= ok -1 = ok
  myreader reader-read-byte 0= ok -1 = ok
  s" ab" compare 0= ok

  s" ab" 2dup 42 myreader init-dbg-reader drop
  myreader reader-peek-byte 0= ok 'a' = ok
  myreader reader-read-byte 0= ok 'a' = ok
  myreader reader-peek-byte 0= ok 'b' = ok
  myreader reader-read-byte 0= ok 'b' = ok
  myreader reader-peek-byte 42 = ok 0= ok
  myreader reader-peek-byte 42 = ok 0= ok
  myreader reader-read-byte 42 = ok 0= ok
  s" ab" compare 0= ok
;

: test-read-sloppy ( -- )
  s" abcdefghijk" 0 myreader init-dbg-reader drop
  pad 5 0 fill
  pad 5 myreader reader-read-sloppy 0= ok
  pad 5 s" abcde" compare 0= ok

  s" abcdefghijk" 42 myreader init-dbg-reader drop
  pad 20 0 fill
  pad 20 myreader reader-read-sloppy 0<> ok
;

: space? ( char -- flag ) 'SPACE' = ;
: non-space? ( char -- flag ) 'SPACE' <> ;

: test-skip ( -- )
  s"     abcd  " 0 myreader init-dbg-reader drop
  myreader reader-peek-byte throw 'SPACE' = ok
  ['] space? myreader reader-skip
  myreader reader-peek-byte throw 'a' = ok
  ['] non-space? myreader reader-skip
  myreader reader-peek-byte throw 'SPACE' = ok
  ['] space? myreader reader-skip
  myreader reader-peek-byte throw -1 = ok
  ['] space? myreader reader-skip
  myreader reader-peek-byte throw -1 = ok

  s"     abcd  " 42 myreader init-dbg-reader drop
  myreader reader-peek-byte throw 'SPACE' = ok
  ['] space? myreader reader-skip
  myreader reader-peek-byte throw 'a' = ok
  ['] non-space? myreader reader-skip
  myreader reader-peek-byte throw 'SPACE' = ok
  ['] space? myreader reader-skip
  myreader reader-peek-byte 42 = ok 0= ok
  ['] space? myreader reader-skip
  myreader reader-peek-byte 42 = ok 0= ok
;

: test-looking-at ( -- )
  s" abc" 0 myreader init-dbg-reader drop
  s" abc" myreader %reader-looking-at? 0= ok 0= ok 0= ok

  myreader %reader-buffer buffer-slice s" abc" compare 0= ok
  s" abcd" myreader %reader-looking-at? 0= ok ok 1 = ok
  myreader %reader-buffer buffer-slice s" abc" compare 0= ok
  s" abcd" myreader %reader-looking-at? 0= ok ok 1 = ok
  s" " myreader %reader-looking-at? 0= ok 0= ok 0 = ok

  myreader reader-read-byte throw 'a' = ok
  s" bc" myreader %reader-looking-at? 0= ok 0= ok 0= ok
  s" bcde" myreader %reader-looking-at? 0= ok ok 2 = ok
  myreader %reader-buffer buffer-slice s" bc" compare 0= ok

  s" a" 0 myreader init-dbg-reader drop
  s" abcd" myreader %reader-looking-at? 0= ok ok 3 = ok

  s" " 0 myreader init-dbg-reader drop
  s" " myreader %reader-looking-at? 0= ok 0= ok 0 = ok
  s" abc" myreader %reader-looking-at? 0= ok ok 3 = ok

  \ The string should not be modified.
  s" abcdefghi" 2dup 0 myreader init-dbg-reader drop
  here 8 myreader reader-read-sloppy 0= ok
  s" ijk" myreader %reader-looking-at? 0= ok ok 2 = ok
  s" abcdefghi" compare 0= ok

  s" abc" 42 myreader init-dbg-reader drop
  s" abc" myreader %reader-looking-at? 0= ok 0= ok 0= ok
  s" abcd" myreader %reader-looking-at? 42 = ok 0= ok 1 = ok
  myreader reader-read-byte throw 'a' = ok
  s" bcde" myreader %reader-looking-at? 42 = ok 0= ok 2 = ok
  s" bc" myreader %reader-looking-at? 0= ok 0= ok 0 = ok

;

create line 1024 allot

: test-read-line ( -- )
  s" " 0 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 0= ok ok 0= ok

  s" abc" 0 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 0= ok ok 3 = ok
  line 3 s" abc" compare 0= ok

  s\" \r\n" 0 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 0= ok 0= ok 0 = ok
  line 0 s" " compare 0= ok
  line 1024 myreader reader-read-line-crlf 0= ok ok 0 = ok
  line 0 s" " compare 0= ok

  s\" abc\r\n" 0 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 0= ok 0= ok 3 = ok
  line 3 s" abc" compare 0= ok
  line 1024 myreader reader-read-line-crlf 0= ok ok 0 = ok
  line 0 s" " compare 0= ok

  s\" \r" 0 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 0= ok ok 1 = ok
  line 1 s\" \r" compare 0= ok
  line 1024 myreader reader-read-line-crlf 0= ok ok 0 = ok
  line 0 s" " compare 0= ok

  s\" \r" 0 myreader init-dbg-reader drop
  line 0 myreader reader-read-line-crlf 0= ok 0= ok 0 = ok

  s" " 42 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 42 = ok 0= ok 0= ok
  line 1024 myreader reader-read-line-crlf 42 = ok 0= ok 0= ok

  s" abc" 42 myreader init-dbg-reader drop
  line 1024 myreader reader-read-line-crlf 42 = ok 0= ok 3 = ok
  line 3 s" abc" compare 0= ok
  line 1024 myreader reader-read-line-crlf 42 = ok 0= ok 0= ok

  s\" a\r" 42 myreader init-dbg-reader drop
  line 0 myreader reader-read-line-crlf 0= ok 0= ok 0 = ok
  line 1024 myreader reader-read-line-crlf 42 = ok 0= ok 2 = ok
  line 2 s\" a\r" compare 0= ok
  line 1024 myreader reader-read-line-crlf 42 = ok 0= ok 0 = ok
;


: main ( -- )
  test-read-byte
  test-peek-byte
  test-read-sloppy
  test-skip
  test-looking-at
  test-read-line
;

' main 169 run-tests
