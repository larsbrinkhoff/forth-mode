
\ Used to set up arguments for ?DO loops so that the loop index I
\ points to the current element in the slice ADDR/U.
: bounds ( addr u -- addr2 addr ) over + swap ;

\ Call XT with temporarily allocated memory.
: with-mem ( xt[ a-addr -- ] u -- )
  allocate throw {: mem :}
  mem swap execute
  mem free throw
;

\ Copy the slice C-ADDR/U to freshly allocated memory at A-ADDR/U.
: copy-slice ( c-addr u -- a-addr u )
  {: u :}
  u allocate throw
  tuck u move
  u
;

\ Copy the entire contents of the file with name FILENAME$ to the
\ freshly allocated memory STRING$.
: slurp-file ( filename$ -- string$ )
  r/o open-file throw
  dup file-size throw
  0<> abort" file too big"	( fid size )
  dup allocate throw		( fid size a-addr )
  >r >r
  dup 2r@ rot read-file throw	( fid count ) ( r: a-addr size )
  r@ <> abort" short read"
  close-file throw
  2r>
;
