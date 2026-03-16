
require prelude.fth
require posix.fth
require tap.fth
require zstring.fth

: test-write ( -- )
  1 s\" abc\n" posix-write 4 = ok
  -1 s\" abc\n" posix-write -1 = ok
;

: test-pipe ( -- )
  pad 10 cells -1 fill
  0 pad posix-fd-pair-get -1 = ok
  1 pad posix-fd-pair-get -1 = ok
  pad posix-pipe -1 <> ok
  0 pad posix-fd-pair-get -1 <> ok
  1 pad posix-fd-pair-get -1 <> ok

  1 pad posix-fd-pair-get s" abc" posix-write 3 = ok
  0 pad posix-fd-pair-get here 10  posix-read 3 = ok
  here 3 s" abc" compare 0= ok
;

create a0 %/fd 2 * allot

: test-fd@ ( -- )
  a0 %/fd 2 * 0 fill
  2 a0 c!
  3 a0 %/fd + c!
  a0 %fd@ 2 = ok
  a0 %/fd + %fd@ 3 = ok

  a0 %/fd 2 * -1 fill
  2 a0 c!
  3 a0 %/fd + c!
  a0 %fd@ -1 8 lshift 2 or = ok
  a0 %/fd + %fd@ -1 8 lshift 3 or = ok
;

: test-opendir ( -- )
  s\" test\z" drop posix-opendir {: dir | dirent name :}
  dir -1 <> ok
  begin
    dir posix-readdir to dirent
    dirent 0= if false ok exit then
    dirent posix-dirent-name to name
    name name strlen s" posix.fth" compare 0=
  until
  dir posix-closedir -1 <> ok
;

: test-errno! ( -- )
  123 posix-errno!
  posix-errno 123 = ok
;

\ : test-realpath ( -- )
\   s\" .\z" drop 0 posix-realpath dup 0<> ok
\   dup c@ '/' = ok
\   posix-free
\ ;

: main ( -- )
  18 test-plan
  depth >r
  test-write
  test-fd@
  test-pipe
  test-opendir
  test-errno!
  \ test-realpath
  depth r> = ok
;

main
bye
