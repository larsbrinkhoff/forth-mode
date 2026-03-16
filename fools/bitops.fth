
: bits-per-byte ( -- u ) 8 ;
: bits-per-cell ( -- u ) 1 cells bits-per-byte * ;

: %alignment-mask ( -- u ) 1 cells 1- ;
: cell-aligned? ( addr -- flag ) %alignment-mask and 0= ;
: cell-aligned ( addr -- a-addr ) 1 cells 1- +  %alignment-mask invert and ;
