\ A charset is a bitset for the integers 0..255.  I.e., we only cover
\ utf-8 code units.

require assert.fth
require bitops.fth

$ff constant %max-char
%max-char 1+ bits-per-cell / cells constant /charset

: init-charset ( a-addr -- charset )
  >r
  r@ /charset 0 fill
  r>
;

: charset, ( xt -- ) here /charset allot init-charset swap execute drop ;

: %charset-bitmask ( char -- u ) bits-per-cell mod 1 swap lshift ;
: %charset-offset ( char -- u ) bits-per-cell / cells ;

: charset-add ( char charset -- )
  assert( over %max-char u> 0= )
  over %charset-bitmask >r
  swap %charset-offset +	( cell-addr ) ( r: bitmask )
  dup @ r> or swap !
;

: charset-member? ( char charset -- flag )
  assert( over %max-char u> 0= )
  over %charset-bitmask >r
  swap %charset-offset +	( cell-addr ) ( r: bitmask )
  @ r> and 0<>
;

: charset-add-range ( from to charset -- )
  rot >r swap 1+ r> ?do
    i over charset-add
  loop
  drop
;

: charset-add-string ( string charset -- )
  rot rot 0 ?do	     ( charset c-addr )
    2dup i chars + c@ swap charset-add
  loop
  2drop
;
