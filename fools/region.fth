\ region.fth --- Region based memory allocation.
\
\ The implementation and API is similar to Gnulib's obstacks.

require assert.fth
require bitops.fth
require allocator.fth

begin-structure %/chunk-header
  field:   %chunk-prev		\ pointer to previous chunk
  field:   %chunk-capacity	\ size of this chunk without header
  0 +field %chunk-contents
end-structure

begin-structure /region
  field: %region-chunk	  \ pointer to current chunk(-header)
  field: %region-base	  \ start address of current object (in chunk)
  field: %region-free	  \ first unused address (in chunk)
  field: %region-limit	  \ address after the current chunk
  field: %region-default-chunk-size
  /allocator +field %region-allocator
end-structure

: %init-chunk ( size prev a-addr -- chunk )
  >r
  r@ %chunk-prev !
  %/chunk-header - r@ %chunk-capacity !
  r>
;

: %make-chunk ( size prev region -- chunk )
  {: s p r :}
  s r %region-allocator 2@ allocator-alloc throw
  s p rot %init-chunk
;

: %drop-chunk ( chunk region -- ) %region-allocator 2@ allocator-free throw ;

: %chunk-limit ( chunk -- addr ) dup %chunk-contents swap %chunk-capacity @ + ;

: init-region-with-args ( default-chunk-size allocator a-addr -- region )
  >r
  r@ %region-allocator 2!
  dup r@ %region-default-chunk-size !
  0 r@ %make-chunk		( chunk )
  dup r@ %region-chunk !	( chunk )
  dup %chunk-contents		( chunk contents )
  dup r@ %region-base !		( chunk contents )
  r@ %region-free !		( chunk )
  %chunk-limit r@ %region-limit !
  assert( r@ %region-base @ cell-aligned? )
  r>
;

: %region-alloc-chunk ( u env -- a-addr ) drop allocate throw ;
: %region-free-chunk ( a-addr env -- ) drop free throw ;

: init-region ( a-addr -- region )
  >r
  8192 %/chunk-header -
  default-allocator
  r> init-region-with-args
;

: deinit-region ( region -- )
  {: r :}
  r %region-chunk @
  begin dup while
    dup %chunk-prev @ swap r %drop-chunk
  repeat
  drop
;

: region-room ( region -- u ) dup %region-limit @ swap %region-free @ - ;

: region-object-size ( region -- u )
  dup %region-free @ swap %region-base @ -
;

: %new-chunk-size ( len region -- u2 )
  {: r :}
  r region-object-size dup 3 rshift + + 100 +
  r %region-default-chunk-size @ max cell-aligned ( new-size )
  assert( dup r region-object-size u> )
;

: %drop-old-chunk-if-empty ( old-chunk new-chunk region -- )
  {: old-chunk new-chunk r :}
  r %region-base @ old-chunk %chunk-contents = if
    old-chunk %chunk-prev @ new-chunk %chunk-prev !
    old-chunk r %drop-chunk
  then
;

\ Create a new chunk, move the current partial object to it, and make
\ the new chunk current.  The new chunk should be large enough to hold
\ the current partial object and LEN additional bytes.
: %region-new-chunk ( len region -- )
  >r
  r@ %new-chunk-size		( new-size )
  r@ %region-chunk @		( new-size old-chunk )
  tuck r@ %make-chunk		( old-chunk new-chunk )
  r@ region-object-size r>	{: old-chunk new-chunk obj-size r :}
  r %region-base @ new-chunk %chunk-contents obj-size move
  old-chunk new-chunk r %drop-old-chunk-if-empty
  new-chunk %chunk-contents	  ( new-base )
  dup r %region-base !		  ( new-base )
  obj-size + r %region-free !	  (  )
  new-chunk %chunk-limit r %region-limit !
  new-chunk r %region-chunk !
;

: %region-ensure-room ( u region -- )
  2dup region-room u> if
    %region-new-chunk
  else
    2drop
  then
;

: region-blank ( n region -- )
  over 0< if
    assert( 2dup region-object-size + 0< 0= )
  else
    2dup %region-ensure-room
  then
  %region-free +!
;

\ We don't allow chunks that contain only empty objects, because we
\ want (a) to drop empty chunks when we need to allocate a newer
\ bigger chunk and (b) be able to call REGION-FREE on empty objects.
\ So when asked to create an empty object at the beginning of a chunk,
\ we create an unused but non-empty object first.
: %prepare-for-empty-object ( region -- )
  {: r :}
  r %region-base @ r %region-chunk @ %chunk-contents = if
    r %region-base @ cell+
    dup r %region-base !
    r %region-free !
  then
;

\ Finish the current object and return it's address.
: region-finish ( region -- a-addr )
  >r
  r@ region-object-size 0= if r@ %prepare-for-empty-object then
  r@ %region-base @
  r@ %region-free @ cell-aligned	( old-base new-base )
  assert( dup r@ %region-limit @ u> 0= )
  assert( dup cell-aligned? )
  dup r@ %region-free !		( old-base new-base )
  r> %region-base !		( old-base )
;

: region-alloc ( u region -- a-addr ) tuck region-blank region-finish ;

: %address-within-chunk? ( addr chunk -- flag )
  dup %chunk-contents swap %chunk-capacity @ over + within
;

\ Free the objects after A-ADDR.
: region-free ( a-addr region -- )
  dup %region-chunk @ {: a r chunk :}
  begin
    a chunk %address-within-chunk? 0= while
    chunk dup %chunk-prev @ to chunk
    r %drop-chunk
    assert( chunk )
  repeat
  assert( a chunk %address-within-chunk? )
  chunk r %region-chunk !
  a r %region-base !
  a r %region-free !
  chunk %chunk-limit r %region-limit !
;

\ Free all objects from region REGION.
: region-free-all ( region -- )
  {: r :}
  r %region-chunk @
  begin				( chunk )
    dup %chunk-prev @		( chunk prev )
    dup 0<> while
    swap r %drop-chunk
  repeat			( chunk prev )
  drop
  dup r %region-chunk !
  dup %chunk-contents		( chunk contents )
  dup r %region-base ! r %region-free !
  %chunk-limit r %region-limit !
;

: region-base ( region -- a-addr ) %region-base @ ;
: region-next-free ( region -- addr ) %region-free @ ;

: region-add-byte ( char region -- )
  {: r :}
  1 r %region-ensure-room
  r %region-free @ c!
  1 r %region-free +!
;

: region-add-cell ( x region -- )
  {: r :}
  1 cells r %region-ensure-room
  r %region-free @ !
  1 cells r %region-free +!
;

\ Append the slice C-ADDR/U to the current object.
: region-add-slice ( c-addr u region -- )
  {: a u r :}
  u r %region-ensure-room
  a r %region-free @ u move
  u r %region-free +!
;
