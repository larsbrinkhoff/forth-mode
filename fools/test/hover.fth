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

: %with-lsp ( xt[server client lsp] server client -- )
  over /lsp allocate throw init-lsp {: xt server client lsp :}

  1 json-make-fixnum
  s" initialize" client jsonrpc-make-string
  client jsonrpc-builder >r
  r@ jb-{ s" rootPath" s" ./" r@ jb-str r@ jb-: r@ jb-}
  r> jsonrpc-make-request
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  drop
  server client lsp xt execute
;

: with-lsp ( xt[server client lsp] -- ) ['] %with-lsp with-jsonrpc ;

: fun-with-signature ( x y z -- ) drop drop drop ;
: fun-without-signature ;

: make-hover-request ( id uri$ line col jb -- json )
  {: id uri urilen line col jb :}
  id json-make-fixnum
  s" textDocument/hover" jb jb-str
  jb jb-{
  s" textDocument"
  jb jb-{
  s" uri" uri urilen jb jb-str jb jb-:
  jb jb-}  cr jb jb-:
  s" position" line col jb %lsp-make-pos jb jb-:
  jb jb-}
  jb jsonrpc-make-request
;

: test-hover {: server client lsp -- :}
  client jsonrpc-builder >r
  1 2 3 fun-with-signature
  3 s" file:test/hover.fth" 52 8 r@ make-hover-request
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" result" rot json-ref
  s" contents" rot json-ref
  dup s" value" rot json-ref json-string-slice
  s" fun-with-signature ( x y z -- )" compare 0= ok
  s" language" rot json-ref json-string-slice s" forth" compare 0= ok
  r> drop
;

: test-no-hover {: server client lsp -- :}
  client jsonrpc-builder >r
  fun-without-signature
  3 s" file:test/hover.fth" 69 10 r@ make-hover-request
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" result" rot json-ref json-null? ok

  \ unknown-fun
  3 s" file:test/hover.fth" 78 10 r@ make-hover-request
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" result" rot json-ref json-null? ok

  r> drop
;

: main ( -- )
  ['] test-hover with-lsp
  ['] test-no-hover with-lsp
;

' main 10 run-tests
