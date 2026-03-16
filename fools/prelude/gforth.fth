\ Some Forth 2012 compatibility for Gforth 0.7.3

[undefined] synonym [if]
: synonym ( "new" "old" -- )
  >in @ parse-name sfind dup if nip then >r ' >in @
  ( pos-new old-xt pos-end ) ( r: imm )
  >r swap >in ! alias r> >in !
  r> 1 = if immediate then
;
[then]

[undefined] n>r [if]
: n>r ( x1 .. xn n -- r:xn..x1 r:n )
    scope r> { n ret }
    0  BEGIN  dup n <  WHILE  swap >r 1+  REPEAT  >r
    ret >r endscope ;
[then]

[undefined] nr> [if]
: nr> ( r:xn..x1 r:n -- x1 .. xn n )
    scope r> r> { ret n }
    0  BEGIN  dup n <  WHILE  r> swap 1+  REPEAT
    ret >r endscope ;
[then]

[undefined] holds [if]
: holds ( addr u -- )
    \ like HOLD, but for a string
    tuck + swap 0 +do
	1- dup c@ hold
    loop
    drop ;
[then]

[undefined] {: ( :} ) [if]
: parse-name1 ( "name" -- c-addr u )
    parse-name dup 0= -16 and throw ;

: parse-rest ( "name" ... "namen" -- )
    begin
        parse-name1
    s" :}" compare 0= until ;

: {helper ( f "name1"..."namen" -- )
    parse-name1
    2dup s" |" compare 0= if
        2drop drop true parse-name1 then
    2dup s" --" compare 0= if
	2drop drop parse-rest exit then
    2dup s" :}" compare 0= if
        2drop drop exit then
    rot dup if \ we are in the | part
        0 postpone literal then
    recurse (local) ;

: {: ( "X:locals definition" -- )
    false {helper 0 0 (local) ; immediate
[then]
