\ mem-reader.fth --- A reader interface for a constant string.
\
\ This is primarily useful for debugging.

require reader.fth

begin-structure /mem-reader
  /reader +
end-structure

: init-mem-reader ( c-addr u a-addr -- reader )
  >r 0 0 2swap dup r> init-reader-with-buffer
;

: make-mem-reader ( c-addr u a-addr -- reader )
  /mem-reader allocate throw init-mem-reader
;

: drop-mem-reader ( reader -- ) free throw ;
