require prelude.fth \ abc
require gapbuffer.fth
require tap.fth
require string.fth

create textbuf /gapbuffer allot

: test-init ( -- )
  1025 textbuf init-gapbuffer
  deinit-gapbuffer
;

: test-copy ( -- )
  1025 textbuf init-gapbuffer
  %gapbuffer-copy s" " compare 0= ok
;

: test-insert ( -- )
  1025 textbuf init-gapbuffer {: b :}
  s" abc" 0 b gapbuffer-insert
  b %gapbuffer-copy s" abc" compare 0= ok
  s" xyz" 1 b gapbuffer-insert
  b %gapbuffer-copy s" axyzbc" compare 0= ok

  1 textbuf init-gapbuffer drop
  s" abc" 0 b gapbuffer-insert
  s" xyz" 3 b gapbuffer-insert
  b %gapbuffer-copy s" abcxyz" compare 0= ok
;

: test-grow ( -- )
  0 textbuf init-gapbuffer {: b :}
  b %gapbuffer-capacity @ 0= ok
  b %gapbuffer-gap-start @ 0= ok
  b %gapbuffer-gap-end @ 0= ok
  100 b %gapbuffer-grow
  b %gapbuffer-capacity @ 100 = ok
  b %gapbuffer-gap-start @ 0 = ok
  b %gapbuffer-gap-end @ 100 = ok
;

: test-make-from-file ( -- )
  s" test/gapbuffer.fth" make-gapbuffer-from-file
  %gapbuffer-copy s" require prelude.fth \ abc" 2swap string-prefix? ok
;

create mem 100 allot

: test-read ( -- )
  0 textbuf init-gapbuffer {: b :}
  s" abc" 0 b gapbuffer-insert
  mem 100 'A' fill
  mem 100 0 b gapbuffer-read mem swap s" abc" compare 0= ok
  mem 100 1 b gapbuffer-read mem swap s" bc" compare 0= ok
  mem 100 2 b gapbuffer-read mem swap s" c" compare 0= ok
  mem 100 3 b gapbuffer-read mem swap s" " compare 0= ok

  s" xyz" 1 b gapbuffer-insert

  mem 100 'A' fill
  mem 100 0 b gapbuffer-read mem swap s" axyzbc" compare 0= ok
  mem 6 + 6 s" AAAAAA" compare 0= ok

  mem 100 'A' fill
  mem 100 5 b gapbuffer-read mem swap s" c" compare 0= ok
  mem 1 + 6 s" AAAAAA" compare 0= ok

  mem 100 'A' fill
  mem 2 1 b gapbuffer-read mem swap s" xy" compare 0= ok
  mem 2 + 6 s" AAAAAA" compare 0= ok
;

: test-line+ ( -- )
  0 textbuf init-gapbuffer {: b :}
  s\" abc\nklm\nuvw\nxyz" 0 b gapbuffer-insert
  0 0 b %gapbuffer-lines+ 0= ok 0= ok
  0 2 b %gapbuffer-lines+ 2 = ok 0= ok
  0 3 b %gapbuffer-lines+ 3 = ok 0= ok
  0 4 b %gapbuffer-lines+ 4 = ok 0= ok

  1 0 b %gapbuffer-lines+ 4 = ok 0= ok
  1 3 b %gapbuffer-lines+ 4 = ok 0= ok
  1 4 b %gapbuffer-lines+ 8 = ok 0= ok

  1000 4 b %gapbuffer-lines+ 15 = ok 998 = ok
;

: test-cols+ ( -- )
  0 textbuf init-gapbuffer {: b :}
  s\" abc\nklm\nuvw\nxyz" 0 b gapbuffer-insert
  0 0 b %gapbuffer-cols+ 0= ok 0= ok
  0 2 b %gapbuffer-cols+ 2 = ok 0= ok
  0 10 b %gapbuffer-cols+ 10 = ok 0= ok

  2 0 b %gapbuffer-cols+ 2 = ok 0= ok
  4 2 b %gapbuffer-cols+ 3 = ok 3 = ok
  4 3 b %gapbuffer-cols+ 3 = ok 4 = ok
  5 10 b %gapbuffer-cols+ 11 = ok 4 = ok
  1000 10 b %gapbuffer-cols+ 11 = ok 999 = ok
;

: test-position>index ( -- )
  0 textbuf init-gapbuffer {: b :}
  s\" abc\nklm\nuvw\nxyz" 0 b gapbuffer-insert
  0 0 b gapbuffer-position>index 0= ok 0= ok
  0 3 b gapbuffer-position>index 0= ok 3 = ok
  1 3 b gapbuffer-position>index 0= ok 7 = ok
  100 3 b gapbuffer-position>index ok 15 = ok
  1 100 b gapbuffer-position>index ok 7 = ok

  0 textbuf init-gapbuffer to b
  s\" \n1\n2" 0 b gapbuffer-insert
  0 0 b gapbuffer-position>index 0= ok 0= ok
  0 1 b gapbuffer-position>index ok 0= ok
  s" 1" 0 b gapbuffer-insert
  0 1 b gapbuffer-delete
  b %gapbuffer-copy s\" \n1\n2" compare 0= ok
  0 0 b gapbuffer-position>index 0= ok 0= ok
  0 1 b gapbuffer-position>index ok 0= ok
  1 0 b gapbuffer-position>index 0= ok 1 = ok
  1 1 b gapbuffer-position>index 0= ok 2 = ok
;

: test-delete ( -- )
  0 textbuf init-gapbuffer {: b :}
  s" abcdefgh" 0 b gapbuffer-insert
  2 4 b gapbuffer-delete
  b %gapbuffer-copy s" abefgh" compare 0= ok

  0 textbuf init-gapbuffer drop
  s" abcdefgh" 0 b gapbuffer-insert
  0 8 b gapbuffer-delete
  b %gapbuffer-copy s" " compare 0= ok

  0 textbuf init-gapbuffer drop
  s" abcdefgh" 0 b gapbuffer-insert
  0 7 b gapbuffer-delete
  b %gapbuffer-copy s" h" compare 0= ok

  0 textbuf init-gapbuffer drop
  s" abcdefgh" 0 b gapbuffer-insert
  7 8 b gapbuffer-delete
  b %gapbuffer-copy s" abcdefg" compare 0= ok

  0 textbuf init-gapbuffer drop
  s\" 0\n1\n2\n" 0 b gapbuffer-insert
  0 0 b gapbuffer-position>index throw
  0 1 b gapbuffer-position>index throw
  b gapbuffer-delete
  b %gapbuffer-copy s\" \n1\n2\n" compare 0= ok
  0 0 b gapbuffer-position>index throw
  1 0 b gapbuffer-position>index throw
  b gapbuffer-delete
  b %gapbuffer-copy s\" 1\n2\n" compare 0= ok
  0 0 b gapbuffer-position>index throw
  1 0 b gapbuffer-position>index throw
  b gapbuffer-delete
  b %gapbuffer-copy s\" 2\n" compare 0= ok

;

: test-change ( -- )
  0 textbuf init-gapbuffer {: b :}
  s\" abcd\nefgh" 0 b gapbuffer-insert
  0 5 s" " b gapbuffer-change
  b %gapbuffer-copy s" efgh" compare 0= ok
;

: main ( -- )
  test-init
  test-copy
  test-insert
  test-grow
  test-make-from-file
  test-read
  test-line+
  test-cols+
  test-position>index
  test-delete
  test-change
;

' main 85 run-tests
