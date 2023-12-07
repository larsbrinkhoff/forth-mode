require assert.fth

begin-structure /buffer
  field: %buffer-start	    \ START and END are offsets in the buffer;
  field: %buffer-end	    \ START is inclusive and END exclusive.
  field: %buffer-base	    \ Address of the memory region.
  field: %buffer-capacity   \ Size of the memory region.
end-structure

: init-buffer ( c-addr u a-addr -- buffer )
  >r
  r@ %buffer-capacity !
  r@ %buffer-base !
  0 r@ %buffer-start !
  0 r@ %buffer-end !
  r>
;

: buffer-remaining ( buffer -- u ) dup %buffer-end @ swap %buffer-start @ - ;
: buffer-empty? ( buffer -- flag ) buffer-remaining 0= ;

: %buffer-start-addr ( buffer -- addr )
  dup %buffer-base @ swap %buffer-start @ +
;

: buffer-put-slice ( c-addr u buffer -- )
  assert( 2dup buffer-remaining u> 0= )
  >r tuck			( u c-addr u ) ( r: buffer )
  r@ %buffer-start-addr swap move
  r> %buffer-start +!
;

: buffer-put-byte ( char buffer -- )
  assert( dup buffer-remaining 0 u> )
  >r
  r@ %buffer-start-addr c!
  1 r> %buffer-start +!
;

: buffer-put-byte-at ( char pos buffer -- )
  assert( 2dup %buffer-capacity @ u> 0= )
  %buffer-base @ + c!
;

: buffer-flip ( buffer -- )
  >r
  r@ %buffer-start @
  0 r@ %buffer-start !
  r> %buffer-end !
;

: buffer-clear ( buffer -- )
  >r
  0 r@ %buffer-start !
  r@ %buffer-capacity @ r> %buffer-end !
;

: buffer-get-slice ( c-addr u buffer -- )
  assert( 2dup buffer-remaining u> 0= )
  >r tuck			( u c-addr u ) ( r: buffer )
  r@ %buffer-start-addr rot rot move
  r> %buffer-start +!
;

: buffer-get-byte ( buffer -- char )
  assert( dup buffer-remaining 0 u> )
  >r
  r@ %buffer-start-addr c@
  1 r> %buffer-start +!
;

: buffer-get-byte-at ( u buffer -- char )
  assert( 2dup %buffer-capacity @ u< )
  %buffer-base @ + c@
;

: buffer-slice ( buffer -- c-addr u )
  dup %buffer-start-addr swap buffer-remaining
;

: buffer-start ( buffer -- u ) %buffer-start @ ;
: buffer-end ( buffer -- u ) %buffer-end @ ;
: buffer-capacity ( buffer -- u ) %buffer-capacity @ ;
: buffer-base ( buffer -- c-addr ) %buffer-base @ ;

: buffer-set-start ( u buffer -- )
  assert( 2dup buffer-end u> 0= )
  %buffer-start !
;

: buffer-set-end ( u buffer -- )
  assert( 2dup buffer-capacity u> 0= )
  %buffer-end !
;

\ Does the buffer begin with the string STRING$?.  REMAINING is the
\ the number of bytes that did not match; REMAINING is 0 if the entire
\ string matched.  EOB? is true if the end-of-buffer was reached; EOB?
\ is only true if REMAINING is non-zero.
: buffer-looking-at? ( string$ buffer -- remaining eob? )
  buffer-slice 2>r
  begin				( string$ ) ( r: slice$ )
    dup 0= if 2drop 2r> 2drop 0 false exit then
    r@ 0= if nip 2r> 2drop true exit then
    over c@ 2r@ drop c@ <> if nip 2r> 2drop false exit then
    1 /string 2r> 1 /string 2>r
  again
;