\ lsp.fth --- LSP server

require jsonrpc.fth
require db.fth
require indexer.fth
require textdoc.fth

begin-structure /lsp
  1 cells  +field %lsp-jsonrpc
  1 cells  +field %lsp-db
  /indexer +field %lsp-indexer
  /vector  +field %lsp-textdocs	   \ vector<textdoc>
end-structure

: %%lsp-initialize ( params lsp -- )
  {: p l :}
  s" rootPath" p json-object-get if
    ." rootPath: " dup .json cr
    json-string-slice
    l %lsp-db @ l %lsp-indexer init-indexer
    indexer-process-directory
  then
;

2 constant TextDocumentSyncKind.Incremental

: %lsp-make-docsync-caps ( jb -- json )
  {: j :}
  j jb-{
  s" openClose" json-true j jb-:
  s" change" TextDocumentSyncKind.Incremental json-make-fixnum j jb-:
  s" willSave" json-false  j jb-:
  s" willSaveWaitUntil" json-false j jb-:
  s" save" json-false j jb-:
  j jb-}
;

: %lsp-make-capabilities ( jsonrcp -- json )
  {: j :}
  j jb-{
    s" capabilities"
    j jb-{
      s" definitionProvider" json-true j jb-:
      s" textDocumentSync" j %lsp-make-docsync-caps j jb-:
    j jb-} j jb-:
  j jb-}
;

: %lsp-initialize ( request lsp -- shutdown? )
  dup %lsp-jsonrpc @ {: r l j :}
  \ ." %lsp-initialize: " r .json cr
  s" params" r json-ref l %%lsp-initialize
  j jsonrpc-builder  %lsp-make-capabilities
  r j jsonrpc-send-response
  false
;

: %lsp-shutdown ( request lsp -- shutdown? )
  %lsp-jsonrpc @ {: r j :}
  json-null r j jsonrpc-send-response
  true
;
: %lsp-find-textdoc ( uri$ lsp -- textdoc|0 )
  {: a u l :}
  l %lsp-textdocs vector-bounds ?do
    i textdoc-uri a u uri= if i unloop exit then
    /textdoc
  +loop
  0
;

: %lsp-with-reader-for-uri ( xt[reader -- ] uri$ lsp -- )
  {: xt a u l :}
  a u l %lsp-find-textdoc ?dup if
    make-textdoc-reader >r
    r@ xt catch
    r> drop-textdoc-reader
    throw
  else
    a u uri>filename make-file-reader >r
    r@ xt catch
    r> drop-file-reader
    throw
  then
;

: %lsp-valid-position? ( uri$ line col lsp -- flag )
  {: a u line col l :}
  a u l %lsp-find-textdoc ?dup if
    line col rot textdoc-valid-position?
  else
    true
  then
;

: %%lsp-word-at ( line col reader -- word$ )
  make-parser {: p :}
  p parser-word-at
  p drop-parser
;

: %lsp-word-at ( uri$ line col lsp -- word$ )
  {: uri urilen line col l :}
  line col ['] %%lsp-word-at uri urilen l %lsp-with-reader-for-uri
;

: %lsp-make-pos ( line col jb -- json )
  {: line col j :}
  j jb-{
  s" line" line json-make-fixnum j jb-:
  s" character" col json-make-fixnum j jb-:
  j jb-}
;

: %lsp-make-range ( start end jb -- json )
  {: start end j :}
  j jb-{
  s" start" start j jb-:
  s" end" end j jb-:
  j jb-}
;

: %lsp-definition-start-pos ( definition jb -- json )
  {: d j :}
  d %definition-line @
  d %definition-column @ d %definition-name @ %db-string-slice nip -
  j %lsp-make-pos
;

: %lsp-definition-end-pos ( definition jb -- json )
  {: d j :}
  d %definition-line @
  d %definition-column @
  j %lsp-make-pos
;

: %lsp-definition-range ( definition jb -- json )
  {: d j :}
  d j %lsp-definition-start-pos
  d j %lsp-definition-end-pos
  j %lsp-make-range
;

: %lsp-definitions>locations ( vector<definition> jb -- json )
  {: v j :}
  j jb-[
  v vector-bounds ?do
    j jb-{
    s" uri" i %definition-uri @ %db-string-slice j jb-str j jb-:
    s" range" i j %lsp-definition-range j jb-:
    j jb-}
    j jb-,
    /definition
  +loop
  j jb-]
;

: %lsp-send-invalid-position ( request jsonrpc -- )
  dup jsonrpc-builder {: r j jb :}
  s" id" r json-ref
  JSONRPC_INVALID_PARAMS json-make-fixnum
  s" Invalid params" jb jb-str
  jb jb-{
  s" message" s" Invalid position" jb jb-str jb jb-:
  s" position" s" params" r json-ref s" position" rot json-ref jb jb-:
  jb jb-}
  j jsonrpc-send-error
;

: %%lsp-definition ( uri$ line col request lsp -- vector<definition>|0 )
  {: uri urilen line col r l :}
  uri urilen line col l %lsp-valid-position? 0= if
    r l %lsp-jsonrpc @ %lsp-send-invalid-position
    0 exit
  then
  uri urilen line col l %lsp-word-at
  ." word-at => " 2dup type cr
    l %lsp-db @ db-select-definitions
;

: %lsp-definition ( request lsp -- shutdown? )
  dup %lsp-jsonrpc @ {: r l j :}
  s" params" r json-ref s" textDocument" rot json-ref s" uri" rot json-ref
  json-string-slice
  s" params" r json-ref s" position" rot json-ref s" line" rot json-ref
  json-integer-value
  s" params" r json-ref s" position" rot json-ref s" character" rot json-ref
  json-integer-value
  r l %%lsp-definition dup 0= if drop false exit then
  dup j jsonrpc-builder %lsp-definitions>locations swap drop-vector
  ." definition => " dup .json cr
  r j jsonrpc-send-response
  false
;

: %lsp-file-readable? ( c-addr u -- flag )
  r/o open-file if drop false else close-file throw true then
;

: %lsp-didOpen ( request lsp -- shutdown? )
  {: r l :}
  s" params" r json-ref s" textDocument" rot json-ref ( doc )
  s" uri" 2 pick json-ref json-string-slice	      ( doc uri$ )
  ." didOpen: " 2dup type cr
  s" text" 4 roll json-ref json-string-slice	      ( uri$ text$ )
  /textdoc l %lsp-textdocs vector-add-blank init-textdoc
  l %lsp-indexer indexer-process-textdoc
  false
;

: %%lsp-didClose ( doc doc2 -- doc flag ) over = ;

: %lsp-didClose ( request lsp -- shutdown? )
  {: r l :}
  s" params" r json-ref s" textDocument" rot json-ref
  s" uri" rot json-ref json-string-slice ( uri$ )
  ." didClose: " 2dup type cr
  2dup l %lsp-find-textdoc	( uri$ doc )
  dup deinit-textdoc
  ['] %%lsp-didClose /textdoc l %lsp-textdocs vector-delete drop
  uri>filename 2dup %lsp-file-readable? if
    l %lsp-indexer indexer-process-file
  else
    2drop
  then
  false
;

: %lsp-unpack-position ( position -- line col )
  >r s" line" r@ json-ref json-integer-value
  s" character" r> json-ref json-integer-value
;

: %lsp-unpack-range ( range -- start-line start-col end-line end-col )
  >r s" start" r@ json-ref %lsp-unpack-position
  s" end" r> json-ref %lsp-unpack-position
;

2 constant MessageType.Warning

: %lsp-notify-invalid-position ( change doc lsp -- )
  %lsp-jsonrpc @ dup jsonrpc-builder {: ch doc j jb :}
  s" window/showMessage" jb jb-str
  jb jb-{
  s" type" MessageType.Warning json-make-fixnum jb jb-:
  s" message" s" Position for textDocument/didChange notification invalid"
  jb jb-str jb jb-:
  jb jb-}
  j jsonrpc-send-notification
  ." invalid position: " ch .json cr
;

: %lsp-apply-change ( change doc lsp -- )
  {: ch doc lsp :}
  \ ." apply-change: " ch .json cr
  \ ." text before: <" cr doc .textdoc-text ." §" cr
  s" range" ch json-object-get if
    %lsp-unpack-range
    s" text" ch json-ref json-string-slice doc textdoc-change
    if ch doc lsp %lsp-notify-invalid-position then
  else
    true abort" nyi"
  then
  \ ." text after: <" cr doc .textdoc-text ." §" cr
;

\ FIXME: delay indexing until it is needed
: %lsp-apply-changes  ( changes textdoc lsp -- )
  {: cs doc lsp :}
  cs json-array-length 0 ?do
    i cs json-array-get doc lsp %lsp-apply-change
  loop
  doc lsp %lsp-indexer indexer-process-textdoc
;

: %lsp-didChange ( request lsp -- shutdown? )
  {: r l :}
  s" params" r json-ref			    ( params )
  dup s" textDocument" rot json-ref	    ( params doc )
  swap s" contentChanges" rot json-ref	    ( doc changes )
  swap s" uri" rot json-ref json-string-slice ( changes uri$ )
  ." didChange: " 2dup type cr
  l %lsp-find-textdoc		( changes doc )
  assert( dup 0<> )
  l %lsp-apply-changes
  false
;

: %lsp-register-methods ( lsp -- )
  dup %lsp-jsonrpc @ {: lsp j :}
  s" initialize" ['] %lsp-initialize lsp j jsonrpc-register-method
  s" shutdown" ['] %lsp-shutdown lsp j jsonrpc-register-method
  s" textDocument/definition" ['] %lsp-definition lsp j jsonrpc-register-method
  s" textDocument/didOpen"
  ['] %lsp-didOpen lsp j jsonrpc-register-notification
  s" textDocument/didChange"
  ['] %lsp-didChange lsp j jsonrpc-register-notification
  s" textDocument/didClose"
  ['] %lsp-didClose lsp j jsonrpc-register-notification
;

: init-lsp ( jsonrpc a-addr -- lsp )
  >r
  r@ %lsp-jsonrpc !
  r@ %lsp-textdocs init-vector drop
  r@ %lsp-register-methods
  make-db r@ %lsp-db !
  r>
;
