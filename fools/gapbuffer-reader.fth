
require reader.fth
require gapbuffer.fth

begin-structure %/gapbuffer-reader
  /reader +
  field: %gapbuffer-reader-gapbuffer
  field: %gapbuffer-reader-offset
end-structure

: %gapbuffer-reader-read ( c-addr u gapbuffer-reader -- u2 ior )
  {: r :}
  r %gapbuffer-reader-offset @ r %gapbuffer-reader-gapbuffer @
  gapbuffer-read
  dup r %gapbuffer-reader-offset +!
  0
;

: %init-gapbuffer-reader ( gapbuffer a-addr -- reader )
  >r
  r@ ['] %gapbuffer-reader-read r@ init-reader drop
  r@ %gapbuffer-reader-gapbuffer !
  0 r@ %gapbuffer-reader-offset !
  r>
;

: make-gapbuffer-reader ( gapbuffer -- reader )
  %/gapbuffer-reader allocate throw
  %init-gapbuffer-reader
;

: drop-gapbuffer-reader ( reader -- )
  dup deinit-reader
  free throw
;
