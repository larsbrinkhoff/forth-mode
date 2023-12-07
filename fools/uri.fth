\ uri.fth -- URI operations
\
\ FIXME: Quick&dirty code.  Needs proper parsing.

require string.fth

: %invalid-uri-syntax ( uri$ -- )
  ." invalid uri syntax: " type cr
  true abort" invalid-uri-syntax"
;

: uri>scheme ( uri$ -- scheme$ )
  s" file:" 2over string-prefix? if 2drop  s" file" exit then
  %invalid-uri-syntax
;

: %uri-split-scheme ( string$ -- scheme$ string2$ )
  2dup uri>scheme dup >r 2swap r> 1+ /string
;

: %uri-drop-scheme ( uri$ -- string$ ) %uri-split-scheme 2swap 2drop ;

: %uri-'/'= ( char -- flag ) '/' = ;

: %uri-split-host ( string$ -- host$ string2$ )
  s" //" 2over string-prefix? if
    2 /string
    ['] %uri-'/'= rot rot string-split
  else
    s" " 2swap
  then
;

: %uri-drop-host ( string$ -- string2$ ) %uri-split-host 2swap 2drop ;
: uri>host ( uri$ -- host$ ) %uri-drop-scheme %uri-split-host 2drop ;
: uri>path ( uri$ -- path$ ) %uri-drop-scheme %uri-drop-host ;
: uri>filename ( uri$ -- filename$ ) uri>path ;

: uri= ( uri1$ uri2$ -- flag )
  {: u1 l1 u2 l2 :}
  u1 l1 u2 l2 compare 0= if true exit then
  u1 l1 uri>scheme
  u2 l2 uri>scheme compare 0<> if false exit then
  u1 l1 uri>host
  u2 l2 uri>host compare 0<> if false exit then
  u1 l1 uri>path
  u2 l2 uri>path compare 0=
;

: make-file-uri ( filename$ -- uri$ )
  s" file:"  {: f flen s slen :}
  slen flen + allocate throw >r
  s r@ slen move
  f r@ slen + flen move
  r> slen flen +
;

: drop-file-uri ( uri$ -- ) drop free throw ;
