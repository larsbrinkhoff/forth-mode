\ reader.fth -- Buffered input stream

require buffer.fth
require assert.fth
require ascii.fth

begin-structure /reader
  1 cells +field %reader-read-xt ( c-addr u env --  u2 ior )
  1 cells +field %reader-env
  /buffer +field %reader-buffer
end-structure

: %reader-call-read ( c-addr u reader -- u2 ior )
  dup %reader-env @ swap %reader-read-xt @ execute
;

: %reader-refill ( reader -- u ior )
  dup %reader-buffer {: r b :}
  b buffer-clear
  b buffer-capacity 0= if 0 0 exit then
  1 b buffer-set-start \ 1 byte extra room for BUFFER-READ-LINE
  b buffer-slice r %reader-call-read ( u ior )
  over 1+ b buffer-set-end
;

\ On EOF, return -1 0.
: reader-read-byte ( reader  -- char|-1 ior )
  dup %reader-buffer {: r b :}
  b buffer-empty? if
    r %reader-refill		( u ior )
    ?dup if exit then		( u )
    0= if -1 0 exit then
  then
  b buffer-get-byte 0
;

: %reader-unread-byte ( char reader -- )
  %reader-buffer >r r@ buffer-start
  assert( dup 0 u> )
  1-
  dup r@ buffer-set-start
  r> buffer-put-byte-at
;

\ Skip bytes as long XT returns false.
: reader-skip ( xt[char -- flag] reader -- )
  dup %reader-buffer {: xt r b :}
  begin
    begin b buffer-empty? 0= while
      b buffer-get-byte >r
      r@ xt execute 0= if r> r %reader-unread-byte exit then
      r> drop
    repeat
    r %reader-refill		( u ior )
    ?dup if 2drop exit then
    0= if exit then
  again
;

: reader-peek-byte ( reader -- char|-1 ior )
  {: r :}
  r reader-read-byte
  ?dup if exit then
  dup -1 = if 0 exit then
  dup r %reader-unread-byte
  0
;

: %rl-error ( c-addr' u' len ior -- len eof? ior ) 2swap 2drop false swap ;

: %rl-push-char ( c-addr u len byte reader --
		  [len eof ior true | c-addr2 u2 len2 false] )
  {: r :}
  2>r
  dup 0= if
    2drop 2>r r %reader-unread-byte
    false 0 true
  else
    2dup drop r> swap c!
    1 /string r> 1+ false
  then
;

: %rl-cr ( c-addr u len reader --
	   [len2 eof ior true | c-addr2 u2 len2 false] )
  {: r :}
  begin
    r reader-read-byte		( c-addr' u' len' byte ior )
    ?dup if nip %rl-error true exit then ( c-addr' u' len' byte )
    dup -1 = if true abort" nyi" exit then
    dup 'LF' = if drop nip nip false 0 true exit then
    dup 'CR' <> if r %rl-push-char exit then
    r %rl-push-char if true exit then
  again
;

: reader-read-line-crlf ( c-addr u1 reader -- u2 eof? ior )
  {: r :} 0
  begin				( c-addr' u' len )
    r reader-read-byte		( c-addr' u' len byte ior)
    ?dup if nip %rl-error exit then	   ( c-addr' u' len byte )
    dup -1 = if drop nip nip true 0 exit then
    dup 'CR' = if drop r %rl-cr if exit then then ( c-addr' u' len byte)
    r %rl-push-char if exit then
  again
;

-1 1 rshift constant %ior-eof

: %reader-read-sloppy ( c-addr u reader -- ior )
  {: r :}
  begin
    dup 0= if 2drop 0 exit then	( c-addr u )
    2dup r %reader-call-read	( c-addr u u2 ior )
    ?dup if >r drop 2drop r> exit then ( c-addr u u2 )
    dup 0= if drop 2drop %ior-eof exit then
    /string
  again
;

\ Read U bytes.  If an error happens, then we don't care about the
\ number of bytes we have received.
: reader-read-sloppy ( c-addr u reader -- ior )
  dup %reader-buffer {: r b :}
  2dup b buffer-remaining min tuck ( c-addr u u2 c-addr u2  )
  b buffer-get-slice
  /string
  r %reader-read-sloppy
;

: init-reader-with-buffer ( env read-xt c-addr u end a-addr -- reader )
  {: end a :}
  a %reader-buffer init-buffer end swap buffer-set-end
  a %reader-read-xt !
  a %reader-env !
  a
;

4096 10 * constant %reader-default-bufsize

: init-reader ( env read-xt a-addr -- reader )
  >r %reader-default-bufsize dup allocate throw swap 0 r>
  init-reader-with-buffer
;

: deinit-reader ( reader -- ) %reader-buffer buffer-base free throw ;
