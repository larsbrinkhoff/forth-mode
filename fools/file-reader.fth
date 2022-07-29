
require reader.fth

/reader constant /file-reader

: init-file-reader ( fid a-addr -- reader ) ['] read-file swap init-reader ;

: make-file-reader ( c-addr u -- reader )
  r/o open-file throw
  /file-reader allocate throw
  init-file-reader
;

: drop-file-reader ( reader -- )
  dup %reader-env @ close-file throw
  dup deinit-reader
  free throw
;
