
require prelude.fth
require jsonrpc.fth
require lsp.fth

0 constant rpc-in-fd
5 constant rpc-out-fd

: fools-init ( -- jsonrpc )
  /jsonrpc allocate throw >r
  rpc-in-fd rpc-out-fd r@ init-jsonrpc
  /lsp allocate throw init-lsp drop
  r>
;

: welcome ( -- )
  time&date 2drop 2drop drop
  4 mod case
    0 of ." There are two kinds of fools:" cr
	 ." those who can't change their opinions and those who won't." cr
      endof
    1 of ." Arguing with a fool proves there are two." cr endof
    2 of ." The greatest fools are ofttimes more clever" cr
	 ." than the men who laugh at them." cr endof
    3 of ." Better a witty fool than a foolish wit." cr endof
    4 of ." By their own follies they perished, the fools." cr endof
  endcase
;

: main ( -- )
  welcome
  fools-init jsonrpc-process-requests
;

main bye
