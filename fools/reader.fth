\ reader.fth -- Buffered input stream

require buffer.fth
require assert.fth
require ascii.fth

begin-structure /reader
  field:         %reader-read-xt ( c-addr u env --  u2 ior )
  field:         %reader-env
  /buffer +field %reader-buffer
end-structure

: %reader-call-read ( c-addr u reader -- u2 ior )
  dup %reader-env @ swap %reader-read-xt @ execute
;

\ Prepare the buffer BUFFER for reading.  Move the last U bytes in the
\ buffer to the beginning.
: %reader-clear ( u buffer -- )
  {: u b :}
  u 0= if
    b buffer-clear
  else
    b buffer-base b buffer-end u - + u ( preserve$ )
    b buffer-clear
    b buffer-put-slice
    u b buffer-set-start
    b buffer-capacity b buffer-set-end
  then
;

: %reader-refill ( preserve reader -- u ior )
  dup %reader-read-xt @ 0= if 2drop 0 0 exit then
  dup %reader-buffer {: p r b :}
  p b %reader-clear
  assert( p b buffer-capacity u< )
  b buffer-slice r %reader-call-read ( u ior )
  over b buffer-start + b buffer-set-end
  0 b buffer-set-start
;

\ On EOF, return -1 0.
: reader-read-byte ( reader  -- char|-1 ior )
  dup %reader-buffer {: r b :}
  b buffer-empty? if
    0 r %reader-refill		( u ior )
    ?dup if exit then		( u )
    0= if -1 0 exit then
  then
  b buffer-get-byte 0
;

: reader-peek-byte ( reader -- char|-1 ior )
  dup %reader-buffer {: r b :}
  b buffer-empty? if
    0 r %reader-refill		( u ior )
    ?dup if exit then		( u )
    0= if -1 0 exit then
  then
  b buffer-start b buffer-get-byte-at 0
;

\ Skip bytes as long XT returns false.
: reader-skip ( xt[char -- flag] reader -- )
  {: xt r :}
  begin
    r reader-peek-byte
    ?dup if 2drop exit then
    dup -1 = if drop exit then
    xt execute 0= if exit then
    r %reader-buffer buffer-get-byte drop
  again
;

: %reader-looking-at? ( string$ reader -- remaining eof? ior )
  dup %reader-buffer {: r b :}
  begin		     ( string$ )
    2dup b buffer-looking-at? 0= if nip nip false 0 exit then
    >r				( string$ ) ( r: remaining' )
    dup r@ - r %reader-refill
    ?dup if nip nip nip r> false rot exit then
    0= if 2drop r> true 0 exit then
    r> drop
  again
;

: %rl-error ( c-addr' u' len ior -- len eof? ior ) 2swap 2drop false swap ;

: %rl-push-char ( c-addr u len byte -- c-addr2 u2 len2 )
  assert( 2 pick 0<> )
  >r 1+				( c-addr u len+1 ) ( r: byte )
  rot dup r> swap c!		( u len+1 c-addr )
  rot 1 /string rot
;

: reader-read-line-crlf ( c-addr u1 reader -- u2 eof? ior )
  {: r :} 0
  begin				( c-addr' u' len )
    over 0= if nip nip false 0 exit then
    s\" \r\n" r %reader-looking-at? 0= swap 0= and swap 0= and if
      r %reader-buffer dup buffer-start 2 + swap buffer-set-start
      nip nip false 0 exit
    then
    r reader-read-byte		    ( c-addr' u' len byte ior)
    ?dup if nip %rl-error exit then ( c-addr' u' len byte )
    dup -1 = if drop nip nip true 0 exit then
    %rl-push-char
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
