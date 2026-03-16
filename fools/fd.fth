\ fd.fth -- File descriptor functions
\
\ A thin layer on top of posix functions with a more Forth-like
\ interface.

require posix.fth

: %>ior ( x -- ior ) -1 = if posix-errno else 0 then ;

: %temp-failure-retry ( c-addr u fd xt[ fd c-addr u -- x] -- x )
  {: a u fd xt :}
  begin
    fd a u xt execute
    dup -1 <> if exit then
    posix-errno posix-eintr <> if exit then
    drop
  again
;

: fd-write ( c-addr u fd -- u2 ior )
  ['] posix-write %temp-failure-retry dup %>ior
;

: fd-read ( c-addr u fd -- u2 ior )
  ['] posix-read %temp-failure-retry dup %>ior
;

: make-pipe ( -- fd0 fd1 ior )
  pad posix-pipe %>ior ?dup if -1 -1 rot exit then
  0 pad posix-fd-pair-get
  1 pad posix-fd-pair-get
  0
;
