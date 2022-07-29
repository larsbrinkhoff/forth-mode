\ vector.fth --- A vector is a growable array of bytes.

require allocator.fth
require bitops.fth
require assert.fth
require misc.fth

0
1 cells +field %vector-base	 \ pointer to start of array
1 cells +field %vector-capacity	 \ number of allocated bytes
1 cells +field %vector-end	 \ number of used bytes
/allocator +field %vector-allocator
constant /vector

: %vector-alloc-mem ( capacity vector -- )
  >r
  dup r@ %vector-allocator 2@ allocator-alloc throw r@ %vector-base !
  r> %vector-capacity !
;

: %vector-room ( vector -- ) dup %vector-capacity @ swap %vector-end @ - ;

: %vector-grow ( new-size vector -- )
  {: s v :}
  v %vector-base @ s v %vector-allocator 2@ allocator-resize throw
  v %vector-base !
  s v %vector-capacity !
;

: %vector-new-size ( room vector -- u2 )
  %vector-capacity @ tuck + {: capacity len :}
  len len 3 rshift + 13 cells +
  capacity 2* max cell-aligned ( new-size )
  assert( dup len u> )
;

: %vector-ensure-room ( u vector -- )
  2dup %vector-room u> if
    tuck %vector-new-size swap %vector-grow
  else
    2drop
  then
;

: vector-slice ( vector -- a-addr u ) dup %vector-base @ swap %vector-end @ ;
: %vector-next-free ( vector -- addr ) vector-slice + ;

: vector-add-blank ( u vector -- addr )
  {: u v :}
  u v %vector-ensure-room
  v %vector-next-free
  u v %vector-end +!
;

: vector-push-slice ( c-addr u vector -- )
  over swap vector-add-blank swap move
;

: vector-push-cell ( x vector -- )
  assert( dup %vector-next-free cell-aligned? )
  1 cells swap vector-add-blank !
;

: init-vector-with-args ( capacity allocator a-addr -- vector )
  >r
  r@ %vector-allocator 2!
  0 r@ %vector-end !
  r@ %vector-alloc-mem
  r>
;

: init-vector ( a-addr -- vector )
  >r 8 cells default-allocator r> init-vector-with-args
;

: deinit-vector ( vector -- )
  dup %vector-base @ swap %vector-allocator 2@ allocator-free throw
;

: vector-bounds ( vector -- limit base ) vector-slice bounds ;
: vector-length ( vector -- u ) %vector-end @ ;
: vector-base ( vector -- addr ) %vector-base @ ;
: vector-limit ( vector -- addr ) vector-slice + ;

\ Return a fresh vector that contains only those elements for which XT
\ returns true.
: vector-filter ( xt[addr -- flag] size vector -- vector2 )
  assert( 2dup vector-length swap mod 0= )
  /vector over %vector-allocator 2@ allocator-alloc throw
  {: xt size v v2 :}
  0 v %vector-allocator 2@ v2 init-vector-with-args drop
  v vector-bounds ?do
    i xt execute if
      i size v2 vector-push-slice
    then
    size
  +loop
  v2
;

: %move-backward ( base limit offset -- )
  {: base limit offset :}
  offset 0= if exit then
  base base offset - limit base - move
;

\ Delete those elements for which XT returns true.
: vector-delete ( xt[addr -- flag] size vector -- )
  assert( 2dup vector-length swap mod 0= )
  0 0 {: xt size v offset base :}
  v vector-bounds ?do
    i xt execute if
      base if
	base i offset %move-backward
	0 to base
      then
      offset size + to offset
    else
      base 0= if i to base then
    then
    size
  +loop
  base if
    base v vector-limit offset %move-backward
  then
  offset negate v %vector-end +!
;

: drop-vector ( vector -- )
  dup %vector-allocator 2@ 2>r
  dup deinit-vector
  2r> allocator-free throw
;
