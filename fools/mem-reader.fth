\ mem-reader.fth --- A reader interface for a constant string.
\
\ This is primarily useful for debugging.

require reader.fth

/reader constant /mem-reader

: %mem-read ( c-addr u env -- u2 ior ) drop 2drop 0 0 ;

: init-mem-reader ( c-addr u a-addr -- reader )
  >r 0 ['] %mem-read 2swap dup r> init-reader-with-buffer
;

: make-mem-reader ( c-addr u a-addr -- reader )
  /mem-reader allocate throw init-mem-reader
;

: drop-mem-reader ( reader -- ) free throw ;
