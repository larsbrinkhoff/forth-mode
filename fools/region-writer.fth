
require writer.fth
require region.fth

/writer
1 cells +field %region-writer-cell
1 cells +field %region-writer-region
constant /region-writer

: %region-writer-write ( c-addr u env -- u2 ior )
  over {: u :}
  region-add-slice
  u 0
;

: init-region-writer ( region a-addr -- writer )
  {: r a :}
  0 r region-blank
  r a %region-writer-region !
  r ['] %region-writer-write
  a %region-writer-cell 1 cells
  a init-writer-with-buffer
;

: region-writer-finish ( writer -- c-addr u )
  dup writer-flush throw
  %region-writer-region @
  dup region-object-size >r
  region-finish r>
;
