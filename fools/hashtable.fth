\ hashtable.fth --- Hashtable with external chaining

require allocator.fth

begin-structure %/ht-entry
  field: %ht-entry-hash		\ Cached hash of key
  field: %ht-entry-key
  field: %ht-entry-value
  field: %ht-entry-next		\ 0|entry&
end-structure

: %init-ht-entry ( key value hash next a-addr -- entry )
  >r
  r@ %ht-entry-next !
  r@ %ht-entry-hash !
  r@ %ht-entry-value !
  r@ %ht-entry-key !
  r>
;

begin-structure /hashtable
  field: %hashtable-slots     \ [entry&]
  field: %hashtable-capacity  \ number of allocated slots
  field: %hashtable-count     \ number of entries
  field: %hashtable-hash-xt   \ xt: key -- u
  field: %hashtable-=-xt      \ xt: key1 key2 -- flag
  field: %hashtable-threshold \ resize if count raises above threshold
  /allocator +field %hashtable-allocator
end-structure

: %ht-allocate-zeroed ( u hashtable -- a-addr )
  over >r
  %hashtable-allocator 2@ allocator-alloc throw
  dup r> 0 fill
;

create %ht-primes
13 , 19 , 29 , 41 , 59 , 79 , 107 , 149 , 197 , 263 , 347 , 457 , 599 , 787 ,
1031 , 1361 , 1777 , 2333 , 3037 , 3967 , 5167 , 6719 , 8737 , 11369 , 14783 ,
19219 , 24989 , 32491 , 42257 , 54941 , 71429 , 92861 , 120721 , 156941 ,
204047 , 265271 , 344857 , 448321 , 582821 , 757693 , 985003 , 1280519 ,
1664681 , 2164111 , 2813353 , 3657361 , 4754591 , 6180989 , 8035301 ,
10445899 , 13579681 , 17653589 , 22949669 , 29834603 , 38784989 ,
50420551 , 65546729 , 85210757 , 110774011 , 144006217 , 187208107 ,
243370577 , 316381771 , 411296309 , 534685237 , 695090819 , 903618083 ,
1174703521 , 1527114613 , 1837299131 , 2147483647 , -1 ,

: %ht-next-prime ( u -- u2 )
  {: u :}
  %ht-primes
  begin
    dup @
    dup -1 = abort" last prime reached"
    dup u u> if nip exit then
    drop
    cell+
  again
;

\ The number is in %.
80 constant %ht-max-load-factor
: %ht-compute-threshold ( capacity -- u ) %ht-max-load-factor * 100 / ;

: %ht-update-threshold ( hashtable -- )
  dup %hashtable-capacity @ %ht-compute-threshold
  swap %hashtable-threshold !
;

: %ht-alloc-slots ( capacity hashtable -- )
  {: ht :}
  %ht-next-prime
  dup cells ht %ht-allocate-zeroed ht %hashtable-slots !
  ht %hashtable-capacity !
  ht %ht-update-threshold
;

: init-hashtable ( hash-xt =-xt capacity allocator a-addr -- hashtable )
  >r
  r@ %hashtable-allocator 2!
  r@ %ht-alloc-slots
  r@ %hashtable-=-xt !
  r@ %hashtable-hash-xt !
  0 r@ %hashtable-count !
  r>
;

: %ht-drop-chain ( chain hashtable -- )
  {: ht :}
  begin dup 0<> while
    dup %ht-entry-next @
    swap ht %hashtable-allocator 2@ allocator-free throw
  repeat
  drop
;

: %ht-slots-slice ( hashtable -- a-addr u )
  dup %hashtable-slots @ swap %hashtable-capacity @ cells
;

: %ht-drop-chains ( hashtable -- )
  {: ht :}
  ht %ht-slots-slice
  begin dup 0<> while
    over @ ht %ht-drop-chain
    1 cells /string
  repeat
  2drop
;

: deinit-hashtable ( hashtable -- )
  {: ht :}
  \ FIXME: optimize for regions where we don't want to call free.
  \ ht %hashtable-free-xt @ 0= if exit then
  ht %ht-drop-chains
  ht %hashtable-slots @ ht %hashtable-allocator 2@ allocator-free throw
;

: %ht-key-hash ( key hashtable -- hash ) %hashtable-hash-xt @ execute ;
: %ht-index-for ( hash hashtable -- index ) %hashtable-capacity @ mod abs ;
: %ht-slot-at ( index hashtable -- slot ) %hashtable-slots @ swap cells + ;

: %ht-search-chain ( hash key chain =-xt -- entry|0 )
  {: h k e =-xt :}
  begin
    e 0= if 0 exit then
    e %ht-entry-hash @ h = if
      e %ht-entry-key @ k =-xt execute if
	e exit
      then
    then
    e %ht-entry-next @ to e
  again
;

: %ht-make-entry ( key value hash next hashtable -- entry )
  %/ht-entry swap %hashtable-allocator 2@ allocator-alloc throw %init-ht-entry
;

: %ht-relocate-entry ( entry hashtable -- )
  {: e ht :}
  e %ht-entry-hash @ ht %ht-index-for ht %ht-slot-at ( slot )
  dup @ e %ht-entry-next !			     ( slot )
  e swap !
;

: %ht-relocate-chain ( chain hashtable -- )
  {: ht :}
  begin dup 0<> while
    dup %ht-entry-next @ swap	( next entry )
    ht %ht-relocate-entry
  repeat
  drop
;

: %ht-resize ( new-capacity hashtable -- )
  dup %hashtable-slots @ over %hashtable-capacity @
  {: ht old-slots old-capacity :}
  ht %ht-alloc-slots
  old-slots old-capacity cells
  begin dup 0<> while
    over @ ht %ht-relocate-chain
    1 cells /string
  repeat
  2drop
  old-slots ht %hashtable-allocator 2@ allocator-free throw
;

: %ht-grow ( hashtable -- ) dup %hashtable-capacity @ 2 * swap %ht-resize ;

: %ht-maybe-grow ( hashtable -- )
  {: ht :}
  ht %hashtable-count @ ht %hashtable-threshold @ u> if
    ht %ht-grow
  then
;

: hashtable-put ( key value hashtable -- )
  {: k v ht | hash slot chain :}
  k ht %ht-key-hash to hash
  hash ht %ht-index-for ht %ht-slot-at to slot
  slot @ to chain
  hash k chain ht %hashtable-=-xt @ %ht-search-chain dup if
    v swap %ht-entry-value !
  else
    drop
    k v hash chain ht %ht-make-entry slot !
    1 ht %hashtable-count +!
  then
  ht %ht-maybe-grow
;

: %hashtable-ref ( key hashtable -- value-addr flag )
  {: k ht | hash chain :}
  k ht %ht-key-hash to hash
  hash ht %ht-index-for ht %ht-slot-at @ to chain
  hash k chain ht %hashtable-=-xt @ %ht-search-chain dup if
    %ht-entry-value true
  else
    false
  then
;

: hashtable-get ( key hashtable -- value flag )
  %hashtable-ref if @ true else false then
;

: %ht-chain-for-each ( xt[ entry -- ] chain -- xt )
  {: xt e :}
  begin e 0<> while
    e xt execute
    e %ht-entry-next @ to e
  repeat
  xt
;

: %ht-for-each-chain ( xt[slot-addr -- ] hashtable -- )
  %ht-slots-slice {: xt a u :}
  begin u 0<> while
    a @ xt execute
    a u 1 cells /string to u to a
  repeat
;

: %ht-for-each-entry ( xt [ entry -- ] hashtable -- )
  ['] %ht-chain-for-each swap %ht-for-each-chain drop
;

: %ht-call-with-key-and-value ( xt entry -- xt )
  {: xt e :}
  e %ht-entry-key @ e %ht-entry-value @ xt execute
  xt
;

: hashtable-for-each ( xt [ ..a key value ..b ] hashtable --  ..b )
  ['] %ht-call-with-key-and-value swap %ht-for-each-entry drop
;

: %ht-call-with-value ( xt entry -- xt )
  %ht-entry-value @ swap {: xt :}
  xt execute
  xt
;

: hashtable-for-each-value ( xt[ ..a value -- ..b ] hashtable -- ..b )
  ['] %ht-call-with-value swap %ht-for-each-entry drop
;

: hashtable-count ( hashtable -- u ) %hashtable-count @ ;

: make-hashtable ( hash-xt =-xt -- hashtable )
  0 default-allocator /hashtable allocate throw init-hashtable
;

: drop-hashtable ( hashtable -- ) dup deinit-hashtable free throw ;
