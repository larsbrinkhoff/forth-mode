
require prelude.fth
require jsonrpc.fth
require fd.fth
require tap.fth

: test-tchar ( -- )
  'a' %tchar? true = ok
  'a' %not-tchar? false = ok
  'A' %tcharset charset-member? true = ok
  'A' %tchar? true = ok
  'A' %not-tchar? false = ok
  '0' %tchar? true = ok
  '9' %tchar? true = ok
;

: test-split-field-name ( -- )
  s" Content-Length:234" %split-field-name
  s" 234" compare 0= ok
  s" Content-Length" compare 0= ok

  s"  Content-Length:234" %split-field-name
  s"  Content-Length:234" compare 0= ok
  s" " compare 0= ok

  s" Content-Length" %split-field-name
  s" Content-Length" compare 0= ok
  s" " compare 0= ok

  s" :234" %split-field-name
  s" :234" compare 0= ok
  s" " compare 0= ok
;

: test-skip-ows ( -- )
  s\" \t" drop c@ %whitespace? ok
  s\"  " drop c@ %whitespace? ok
  s\" \n" drop c@ %whitespace? 0= ok
  s" abc" %skip-OWS s" abc" compare 0= ok
  s"   abc" %skip-OWS s" abc" compare 0= ok
  s\" \t  abc" %skip-OWS s" abc" compare 0= ok
  s\" " %skip-OWS s" " compare 0= ok
;

: test-parse-content-length ( -- )
  '0' digit? true = ok
  '1' digit? true = ok
  '9' digit? true  = ok
  '9' 1+ digit? false = ok
  '0' 1- digit? false = ok

  s" 123" %parse-content-length
  true = ok
  123 = ok

  s"  123  " %parse-content-length
  true = ok
  123 = ok

  s"  -123  " %parse-content-length
  false = ok
  0 = ok

  s"  123 d " %parse-content-length
  false = ok
  0 = ok

  s" " %parse-content-length
  false = ok
  0 = ok


  s" 7" %parse-content-length
  true = ok
  7 = ok

  s"  7 " %parse-content-length
  true = ok
  7 = ok
;

create jsonrpc /jsonrpc allot

: test-parse-header-part ( -- )
  make-pipe throw	 ( fd0 fd1 )
  swap 1 jsonrpc init-jsonrpc {: fd1 jsonrpc :}

  s\" \r\n" fd1 fd-write throw
  2 = ok
  jsonrpc jsonrpc-parse-header-part
  0 = ok


  s\" Content-Length: 123 \r\n\r\n" tuck fd1 fd-write throw
  = ok
  jsonrpc jsonrpc-parse-header-part
  123 = ok

  s\" Content-Length:1234\r\n\r\n" tuck fd1 fd-write throw
  = ok
  jsonrpc jsonrpc-parse-header-part
  1234 = ok


  s\" \r\n" tuck fd1 fd-write throw
  = ok
  jsonrpc jsonrpc-parse-header-part
  0 = ok

  \ s\" \n" tuck fd1 fd-write throw
  \ = ok
  \ jsonrpc jsonrpc-parse-header-part
  \ 0 = ok
;

: test-read-content ( -- )
  make-pipe throw   ( fd0 fd1 )
  swap 1 jsonrpc init-jsonrpc {: fd1 jsonrpc :}

  s\" Content-Length: 11\r\n\r\n{ content }" fd1 fd-write throw
  33 = ok
  jsonrpc jsonrpc-parse-header-part
  dup 11 = ok
  jsonrpc %jsonrpc-read-content
  2dup s" { content }" compare 0= ok
  2drop
;

\ Some helpers to create strings prefixed with a Content-Length
\ header.

: string, ( c-addr u -- )
  here
  over allot
  swap move
;

: line, ( -- )
  source >in @ /string string, 'LF' c,
  source nip >in !
;

: msg, ( -- header )
  here
  %jsonrpc-header-template string,
;

: msg; ( header -- c-addr u )
  here over -
  2dup %jsonrpc-patch-content-length
;

msg, line, { "jsonrpc":"2.0", "id":123, "method":"ping" }
msg; 2constant req1

msg, line, { "jsonrpc":"2.0", "id":124, "method":"stop" }
msg; 2constant req2

msg, line, {"jsonrpc":"2.0", "id":123, "result":true}
msg; 2constant response1

create mem 1000 allot

: %ping ( request jsonrpc -- shutdown? )
  {: r j :}
  json-true r j jsonrpc-send-response
  false
;

: %shutdown ( request jsonrpc -- shutdown? )
  {: r j :}
  json-true r j jsonrpc-send-response
  true
;

: compare-response ( request response -- )
  make-pipe throw		( pipe1-in pipe1-out )
  make-pipe throw		( pipe1-in pipe1-out pipe2-in pipe2-out )
  >r rot r> jsonrpc init-jsonrpc {: a1 u1 a2 u2 pipe1-out pipe2-in jsonrpc :}

  s" ping" ['] %ping jsonrpc jsonrpc jsonrpc-register-method

  a1 u1 tuck pipe1-out fd-write throw = ok
  jsonrpc jsonrpc-process-request drop
  mem 1000 pipe2-in fd-read throw
  mem swap \ 2dup type
  a2 u2    \ 2dup type
  compare 0= ok
;

: test-ping ( -- )
  req1 response1 compare-response
;

msg, line, { "jsonrpc":"2.0", "id":123true, "method":"ping" }
msg; 2constant req3

msg, line, {"jsonrpc":"2.0", "id":null, "error":{"code":-32700, "message":"parse error"}}
msg; 2constant response3

: test-parse-error ( -- )
  req3 response3 compare-response
;

msg, line, { "jsonrpc":"1.1", "id":123, "method":"ping" }
msg; 2constant req4

msg, line, {"jsonrpc":"2.0", "id":123, "error":{"code":-32600, "message":"invalid request"}}
msg; 2constant response4

msg, line, { "jsonrpc":"2.0", "id":123, "meth":"ping" }
msg; 2constant req5

msg, line, {"jsonrpc":"2.0", "id":123, "error":{"code":-32600, "message":"invalid request"}}
msg; 2constant response5

msg, line, { "jsonrpc":"2.0", "id":123, "method":true }
msg; 2constant req6

msg, line, {"jsonrpc":"2.0", "id":123, "error":{"code":-32600, "message":"invalid request"}}
msg; 2constant response6

: test-invalid-request ( -- )
  req4 response4 compare-response
  req5 response5 compare-response
  req6 response6 compare-response
;

msg, line, { "jsonrpc":"2.0", "id":123, "method":"foo" }
msg; 2constant req7

msg, line, {"jsonrpc":"2.0", "id":123, "error":{"code":-32601, "message":"method not found"}}
msg; 2constant response7

: test-method-not-found ( -- )
  req7 response7 compare-response
;

msg, line, { "jsonrpc":"2.0", "id":124, "method":"ping" }
msg; 2constant req8
msg, line, { "jsonrpc":"2.0", "id":125, "method":"ping" }
msg; 2constant req9
msg, line, { "jsonrpc":"2.0", "id":126, "method":"shutdown" }
msg; 2constant req10

here
s\" Content-Length:      43\r\n\r\n" string,
s\" {\"jsonrpc\":\"2.0\", \"id\":124, \"result\":true}\n" string,
s\" Content-Length:      43\r\n\r\n" string,
s\" {\"jsonrpc\":\"2.0\", \"id\":125, \"result\":true}\n" string,
s\" Content-Length:      43\r\n\r\n" string,
s\" {\"jsonrpc\":\"2.0\", \"id\":126, \"result\":true}\n" string,
here over - 2constant response10

: test-shutdown ( -- )
  make-pipe throw		( pipe1-in pipe1-out )
  make-pipe throw		( pipe1-in pipe1-out pipe2-in pipe2-out )
  >r rot r> jsonrpc init-jsonrpc {: pipe1-out pipe2-in jsonrpc :}

  req8 tuck pipe1-out fd-write throw = ok
  req9 tuck pipe1-out fd-write throw = ok
  req10 tuck pipe1-out fd-write throw = ok

  s" ping"     ['] %ping     jsonrpc jsonrpc jsonrpc-register-method
  s" shutdown" ['] %shutdown jsonrpc jsonrpc jsonrpc-register-method

  jsonrpc jsonrpc-process-requests
  mem 1000 pipe2-in fd-read throw
  mem swap    \ 2dup type
  response10  \ 2dup type
  compare 0= ok
;

: with-jsonrpc ( xt[server client -- ] -- )
  make-pipe throw		( xt pipe1-in pipe1-out )
  make-pipe throw		( xt pipe1-in pipe1-out pipe2-in pipe2-out )
  /jsonrpc allocate throw
  /jsonrpc allocate throw {: xt p1-in p1-out p2-in p2-out server client :}
  p1-in p2-out server init-jsonrpc drop
  p2-in p1-out client init-jsonrpc drop
  server client xt execute
;

: test-client {: server client -- :}
  123 json-make-fixnum
  client jsonrpc-builder >r
  s" ping" r@ jb-str  r@ jb-{ r@ jb-}  r> jsonrpc-make-request
  client %jsonrpc-send-message

  s" ping" ['] %ping server server jsonrpc-register-method
  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  dup s" result" rot json-ref json-true = ok
  s" id" rot json-ref json-integer-value 123 = ok
;

: main ( -- )
  test-tchar
  test-split-field-name
  test-skip-ows
  test-parse-content-length
  test-parse-header-part
  test-read-content
  test-ping
  test-parse-error
  test-invalid-request
  test-method-not-found
  test-shutdown
  ['] test-client with-jsonrpc
;

' main 72 run-tests
