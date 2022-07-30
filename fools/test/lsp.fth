
require prelude.fth
require lsp.fth
require tap.fth

: with-jsonrpc ( xt[server client -- ] -- )
  make-pipe throw		( xt pipe1-in pipe1-out )
  make-pipe throw		( xt pipe1-in pipe1-out pipe2-in pipe2-out )
  /jsonrpc allocate throw
  /jsonrpc allocate throw {: xt p1-in p1-out p2-in p2-out server client :}
  p1-in p2-out server init-jsonrpc drop
  p2-in p1-out client init-jsonrpc drop
  server client xt execute
;

: test-init ( server client -- )
  swap /lsp allocate throw init-lsp {: client lsp :}
;

: initialize ( server client -- lsp )
  over /lsp allocate throw init-lsp {: server client lsp :}
  1 json-make-fixnum
  client jsonrpc-builder >r
  s" initialize" r@ jb-str
  r@ jb-{
  s" rootPath" s" ./" r@ jb-str r@ jb-:
  r@ jb-}
  r> jsonrpc-make-request
  client %jsonrpc-send-message

  server jsonrpc-process-request drop
  server client lsp
;

: test-initialize ( server client -- )
  initialize {: server client lsp :}

  s" test-initialize" lsp %lsp-db @ db-select-definitions
  vector-length 1 /definition * = ok

  client %json-parse-message 0= ok
  s" result" rot json-ref
  s" capabilities" rot json-ref
  s" definitionProvider" rot json-ref json-true = ok
;

: test-textDocument/definition ( server client -- )
  initialize {: server client lsp :}

  s" test-initialize" lsp %lsp-db @ db-select-definitions
  vector-length 1 /definition * = ok

  client %json-parse-message 0= ok
  s" result" rot json-ref
  s" capabilities" rot json-ref
  s" definitionProvider" rot json-ref json-true = ok

  2 json-make-fixnum
  s" textDocument/definition" client jsonrpc-make-string

  client jsonrpc-builder >r
  r@ jb-{
  s" textDocument"
  r@ jb-{
  s" uri" s" file:test/lsp.fth" r@ jb-str r@ jb-: r@ jb-} r@ jb-:
  s" position" 46 15 r@ %lsp-make-pos r@ jb-:
  r@ jb-}
  dup .json cr
  r> jsonrpc-make-request
  client %jsonrpc-send-message

  server jsonrpc-process-request drop
  client %json-parse-message 0= ok
  s" result" rot json-ref
  dup json-array? ok
  dup json-array-length 1 = ok
  0 swap json-array-get
  dup s" uri" rot json-ref json-string-slice
  s" file:./test/lsp.fth" compare 0= ok
  s" range" rot json-ref
  dup s" start" rot json-ref s" line" rot json-ref json-integer-value 46 = ok
  s" end" rot json-ref s" character" rot json-ref json-integer-value 30 = ok
;

: test-shutdown ( server client -- )
  initialize {: server client lsp :}

  client %json-parse-message 0= ok
  s" result" rot json-ref
  s" capabilities" rot json-ref
  s" definitionProvider" rot json-ref json-true = ok

  2 json-make-fixnum
  client jsonrpc-builder >r
  s" shutdown" r@ jb-str
  r@ jb-{ r@ jb-} r> jsonrpc-make-request
  client %jsonrpc-send-message

  lsp %lsp-shutdown-received? @ 0= ok
  server jsonrpc-process-request 0= ok
  lsp %lsp-shutdown-received? @ ok

  client %json-parse-message 0= ok

  s" result" rot json-ref json-null = ok
;

: main ( -- )
  ['] test-init with-jsonrpc
  ['] test-initialize with-jsonrpc
  ['] test-textDocument/definition with-jsonrpc
  ['] test-shutdown with-jsonrpc
;

' main 20 run-tests
