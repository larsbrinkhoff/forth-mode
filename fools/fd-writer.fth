
require writer.fth
require fd.fth

/writer constant /fd-writer

4096 10 * constant %fd-writer-default-bufsize

: init-fd-writer ( fd a-addr -- writer )
  {: a :}
  ['] fd-write
  %fd-writer-default-bufsize dup allocate throw swap
  a init-writer-with-buffer
;

: deinit-fd-writer ( a-addr -- )
  %writer-buffer buffer-base free throw
;

: make-fd-writer ( fd a-addr -- writer )
  /fd-writer allocate throw init-fd-writer
;

: drop-fd-writer ( writer -- ) dup deinit-fd-writer free throw ;
