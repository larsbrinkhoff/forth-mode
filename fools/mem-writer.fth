require writer.fth

/writer constant /mem-writer

: %mem-writer-write ( c-addr u env -- u2 ior ) drop 2drop 0 $bad ;

: init-mem-writer ( c-addr u a-addr -- mem-writer )
  >r 0 ['] %mem-writer-write 2swap r> init-writer-with-buffer
;

: mem-writer-slice ( mem-writer -- c-addr u )
  %writer-buffer dup buffer-base swap buffer-start
;
