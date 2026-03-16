\ writer.fth --- Buffered output stream

require buffer.fth

begin-structure /writer
  field:         %writer-write-xt ( c-addr u writer -- ior )
  field:         %writer-env
  /buffer +field %writer-buffer
end-structure

: %writer-call-write ( c-addr u writer -- u ior )
  dup %writer-env @ swap %writer-write-xt @ execute
;

: %writer-write-all ( c-addr u writer -- u2 ior )
  0 {: w len :}
  begin
    dup 0= if 2drop len 0 exit then
    2dup w %writer-call-write ( c-addr u u' ior )
    ?dup if 2swap 2drop swap len + swap exit then
    dup 0= abort" zero length partial write?"
    dup len + to len
    /string
  again
;

: %writer-flush-buffer ( writer -- ior )
  dup %writer-buffer {: w b :}
  b buffer-flip
  b buffer-slice w %writer-write-all ( u ior )
  swap				     ( ior u )
  dup b buffer-end = if
    drop
    b buffer-clear
  else
    >r
    b buffer-slice r@ /string b buffer-base swap move
    b buffer-end r> - b buffer-set-start
    b buffer-capacity b buffer-set-end
    assert( dup 0<> )
  then
;

: writer-write ( c-addr u writer -- u ior )
  dup %writer-buffer {: w b :}
  dup b buffer-remaining u> 0= if
    tuck b buffer-put-slice
    0
  else
    w %writer-flush-buffer ?dup if >r 2drop 0 r> exit then
    w %writer-call-write
  then
;

: writer-flush ( writer -- ior ) %writer-flush-buffer ;

: writer-write-byte ( char writer -- ior )
  dup %writer-buffer {: w b :}
  b buffer-remaining 0= if
    w %writer-flush-buffer ?dup if nip exit then
  then
  assert( b buffer-remaining 0 u> )
  b buffer-put-byte
  0
;

: init-writer-with-buffer ( env xt c-addr u a-addr -- writer )
  {: w :}
  w %writer-buffer init-buffer buffer-clear
  w %writer-write-xt !
  w %writer-env !
  w
;
