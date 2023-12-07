
require reader.fth
require fd.fth

/reader constant /fd-reader

: init-fd-reader ( fd a-addr -- reader ) ['] fd-read swap init-reader ;

: make-fd-reader ( fd -- reader )
  /fd-reader allocate throw init-fd-reader
;
