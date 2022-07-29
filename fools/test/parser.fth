
require prelude.fth
require parser.fth
require tap.fth
require mem-reader.fth
require file-reader.fth

create mreader /mem-reader allot
create mparser /parser allot

wordlist constant mwordlist
get-current mwordlist set-current

: y ( n env -- n+1 )
  {: p :}
  p %parser-line @ 0 u.r ." :" p %parser-column @ 0 u.r ." : "
  p %parser-token-buffer buffer-slice type cr
  1+
;

set-current

: test1 ( -- )
  s\" abc : bar\nx y z\nw x y" mreader init-mem-reader drop
  mwordlist mparser mparser init-parser drop
  0 mreader mparser parser-parse
  2 = ok
;

: test-word-at ( -- )
  0 0 mparser init-parser drop
  29 7 s" test/parser.fth" make-file-reader mparser parser-word-at
  s" test-word-at" compare 0= ok
  29 13 s" test/parser.fth" make-file-reader mparser parser-word-at
  s" test-word-at" compare 0= ok

  10000 0 s" test/parser.fth" make-file-reader mparser parser-word-at
  s" " compare 0= ok

  29 10000 s" test/parser.fth" make-file-reader mparser parser-word-at
  s" " compare 0= ok

;

: %skip ( terminator char -- terminator|-1 flag )
  over -1 = if 2drop -1 false exit then
  over = if drop -1 true exit then
  true
;

: test-skip ( -- )
  0 0 mparser init-parser drop
  s\" foo \n bar ) baz" make-mem-reader
  mparser %parser-reader !
  0 mparser %parser-line !
  ')' ['] %skip mparser parser-skip
  -1 = ok
  mparser %parser-scan s" baz" compare 0= ok
  mparser %parser-line @ 1 = ok
;

: main ( -- )
  test1
  test-word-at
  test-skip
;

' main 9 run-tests
