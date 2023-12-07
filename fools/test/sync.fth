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

: test-didOpen {: server client lsp -- :}
  s" textDocument/didOpen" client jsonrpc-make-string

  client jsonrpc-builder >r
  r@ jb-{
  s" textDocument"
  r@ jb-{
  s" uri" s" file:./test/sync.fth" r@ jb-str r@ jb-:
  s" text" s" test/sync.fth" slurp-file r@ jb-str r@ jb-:
  r@ jb-} r@ jb-:
  r@ jb-}
  r> jsonrpc-make-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  \ verify that the textdoc exists on the server
  lsp %lsp-textdocs vector-length /textdoc = ok
  lsp %lsp-textdocs vector-slice drop
  dup %textdoc-uri 2@ s" file:./test/sync.fth" compare 0= ok
  %textdoc-gapbuffer %gapbuffer-copy
  s" test/sync.fth" slurp-file compare 0= ok
;

: make-didOpen-notification ( uri$ text$ jb -- json )
  {: uri urilen text textlen jb :}
  s" textDocument/didOpen" jb jb-str
  jb jb-{
  s" textDocument"
  jb jb-{
  s" uri" uri urilen jb jb-str jb jb-:
  s" text" text textlen jb jb-str jb jb-:
  jb jb-} jb jb-:
  jb jb-}
  jb jsonrpc-make-notification

;

: make-didClose-notification ( uri$ jb -- json )
  {: uri urilen jb :}
  s" textDocument/didClose" jb jb-str
  jb jb-{
  s" textDocument" jb jb-{ s" uri" uri urilen jb jb-str jb jb-: jb jb-} jb jb-:
  jb jb-}
  jb jsonrpc-make-notification
;

: make-Range {: start-line start-col end-line end-col jb -- json :}
  start-line start-col jb %lsp-make-pos
  end-line   end-col   jb %lsp-make-pos
  jb %lsp-make-range
;

: make-didChange-notification ( uri urilen version
				start-line start-col
				end-line end-col
				text textlen jb -- json )
  {: u ul v sl sc el ec t tl jb -- json :}
  s" textDocument/didChange" jb jb-str
  jb jb-{

  s" textDocument"
  jb jb-{
  s" uri" u ul jb jb-str jb jb-:
  s" version" v json-make-fixnum jb jb-:
  jb jb-}
  jb jb-:

  s" contentChanges"
  jb jb-[
  jb jb-{
  s" range" sl sc el ec jb make-Range jb jb-:
  s" text" t tl jb jb-str jb jb-:
  jb jb-}
  jb jb-,
  jb jb-]
  jb jb-:

  jb jb-}
  jb jsonrpc-make-notification
;

: make-definition-request ( id uri$ line col jb -- json )
  {: id uri uri-len line col j :}
  id json-make-fixnum
  s" textDocument/definition" j jb-str
  j jb-{
  s" textDocument" j jb-{ s" uri" uri uri-len j jb-str j jb-: j jb-} j jb-:
  s" position" line col j %lsp-make-pos j jb-:
  j jb-}
  j jsonrpc-make-request
;

: test-word-at {: server client lsp -- :}
  s" file:foo.fth"
  s" abc frob ghi : frob ; foo "
  client jsonrpc-builder >r
  r@ make-didOpen-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" file:foo.fth" 0 5 lsp %lsp-word-at s" frob" compare 0= ok

  2 s" file:foo.fth" 0 5 r> make-definition-request

  client %jsonrpc-send-message
  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" result" rot json-ref
  0 swap json-array-get
  s" uri" rot json-ref json-string-slice s" file:foo.fth" compare 0= ok

;

: test-didChange {: server client lsp -- :}
  s" file:foo.fth"
  s" ( : frob ; ) frob "
  client jsonrpc-builder >r
  r@ make-didOpen-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" file:foo.fth" 0 15 lsp %lsp-valid-position? ok
  s" file:foo.fth" 0 15 lsp %lsp-word-at s" frob" compare 0= ok

  2 s" file:foo.fth" 0 15 r@ make-definition-request
  client %jsonrpc-send-message
  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" result" rot json-ref json-array-length 0= ok

  s" file:foo.fth" 2 0 0 0 17
  s" : frob ; " r@ make-didChange-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" file:foo.fth" lsp %lsp-find-textdoc %textdoc-gapbuffer
  %gapbuffer-copy s" : frob ;  " compare 0= ok

  2 s" file:foo.fth" 0 5 r> make-definition-request
  client %jsonrpc-send-message
  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" result" rot json-ref
  json-array-length 1 = ok

;

: test-definition-invalid-position {: server client lsp -- :}
  client jsonrpc-builder >r

  s" file:foo.fth" s" foo" r@ make-didOpen-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" file:foo.fth" 0 15 lsp %lsp-valid-position? 0= ok

  2 s" file:foo.fth" 0 15 r> make-definition-request
  client %jsonrpc-send-message
  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" error" rot json-ref
  dup s" message" rot json-ref
  json-string-slice s" Invalid params" compare 0= ok
  s" data" rot json-ref s" message" rot json-ref
  json-string-slice s" Invalid position" compare 0= ok
;

: test-didChange-invalid-position {: server client lsp -- :}
  client jsonrpc-builder >r

  s" file:foo.fth" s" foo" r@ make-didOpen-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" file:foo.fth" 0 3 lsp %lsp-valid-position? ok
  s" file:foo.fth" 0 4 lsp %lsp-valid-position? 0= ok

  s" file:foo.fth" 2 0 0 0 5
  s" : frob ; " r> make-didChange-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  client %json-parse-message 0= ok
  s" method" rot json-ref
  json-string-slice s" window/showMessage" compare 0= ok
;

: test-didClose {: server client lsp -- :}
  client jsonrpc-builder >r

  s" file:foo.fth" s\" foo\nbar" r@ make-didOpen-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" file:foo.fth" 0 3 lsp %lsp-valid-position? ok

  s" file:foo.fth" r> make-didClose-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop
  s" file:foo.fth" lsp %lsp-find-textdoc 0= ok
;

: test-didClose2 ( server client lsp -- )
  over jsonrpc-builder {: server client lsp jb :}

  s" test-didClose2" lsp %lsp-db @ db-select-definitions
  dup vector-length /definition = ok

  vector-base %definition-uri @ %db-string-slice
  2dup s" test/sync.fth" string-suffix? ok
  2>r				( r: uri$ )

  2r@ s" test/sync.fth" slurp-file jb make-didOpen-notification
  client %jsonrpc-send-message

  server jsonrpc-process-request drop

  s" test-didClose2" lsp %lsp-db @ db-select-definitions
  vector-length /definition = ok

  2r@ jb make-didClose-notification client %jsonrpc-send-message

  server jsonrpc-process-request drop
  2r@ lsp %lsp-find-textdoc 0= ok

  \ Stale definitions should be have been removed
  s" test-didClose2" lsp %lsp-db @ db-select-definitions
  vector-length /definition = ok

  2r> 2drop
;

: main  ( -- )
  ['] test-didOpen with-lsp
  ['] test-word-at with-lsp
  ['] test-didChange with-lsp
  ['] test-definition-invalid-position with-lsp
  ['] test-didChange-invalid-position with-lsp
  ['] test-didClose with-lsp
  ['] test-didClose2 with-lsp
;

' main 36 run-tests
