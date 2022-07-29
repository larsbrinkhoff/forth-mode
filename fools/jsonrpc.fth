\ jsonrpc.fth --- JSON-RPC server

require fd-reader.fth
require fd-writer.fth
require charset.fth
require string.fth
require region.fth
require json-parser.fth
require json-write.fth
require json-util.fth
require region-writer.fth
require hashtable.fth
require hash.fth
require region-allocator.fth

begin-structure /jsonrpc
  /fd-reader +field %jsonrpc-reader
  /fd-writer +field %jsonrpc-writer
  1 cells +field %jsonrpc-content-length
  /region +field %jsonrpc-request-region \ region for per-request objects
  /json-parser   +field %jsonrpc-parser
  /region-writer +field %jsonrpc-response-writer
  /region    +field %jsonrpc-permanent-region
  /hashtable +field %jsonrpc-methods       \ hashtable<json-string,method>
  /hashtable +field %jsonrpc-notifications \ hashtable<json-string,method>
end-structure

begin-structure %/method
  1 cells +field %method-xt   \ xt: request env -- shutdown?
  1 cells +field %method-name \ json string
  1 cells +field %method-env
end-structure

: %init-method ( name xt env a-addr -- method )
  >r
  r@ %method-env !
  r@ %method-xt !
  r@ %method-name !
  r>
;

-32700 constant JSONRPC_PARSE_ERROR
-32600 constant JSONRPC_INVALID_REQUEST
-32601 constant JSONRPC_METHOD_NOT_FOUND
-32602 constant JSONRPC_INVALID_PARAMS
-32603 constant JSONRPC_INTERNAL_ERROR

: %/pad ( -- ) [ s" /pad" environment? 0= [if] 80 [then] ] literal ;

: %init-tcharset ( a-addr -- charset )
  init-charset >r
  'A' 'Z' r@ charset-add-range
  'a' 'z' r@ charset-add-range
  '0' '9' r@ charset-add-range
  s" !#$%&'*+-.^_`|~" r@ charset-add-string
  r>
;

create %tcharset /charset allot %tcharset %init-tcharset drop

: %tchar? ( char -- flag ) %tcharset charset-member? ;
: %not-tchar? ( char -- flag ) %tchar? 0= ;
: %whitespace? ( char -- flag ) dup 'SPACE' = swap 'HTAB' = or ;
: %not-whitespace? ( char -- flag ) %whitespace? 0= ;
: %not-digit? ( char -- flag ) digit? 0= ;

: %split-field-name ( string -- field-name string2 )
  2dup
  ['] %not-tchar? rot rot string-split ( string field-name :string2 )
  dup 0= if 2drop 2drop s" " 2swap exit then
  2over nip 0= if 2drop 2drop s" " 2swap exit then
  over c@ ':' <> if 2drop 2drop s" " 2swap exit then
  1 /string
  2rot 2drop
;

: %skip-OWS ( string -- string2 )
  ['] %not-whitespace? rot rot string-split 2swap 2drop
;

: %parse-content-length ( string -- u ok? )
  %skip-OWS
  ['] %not-digit? rot rot string-split
  %skip-OWS			( digits rest )
  dup 0<> if 2drop 2drop 0 false exit else 2drop then ( digits )
  dup 0= if 2drop 0 false exit then
  0 0 2swap >number 2drop
  d>s true
;

: %jsonrpc-parse-content-length ( string jsonrpc -- ok? )
  {: jsonrpc :}
  %parse-content-length 0= if drop false exit then
  jsonrpc %jsonrpc-content-length !
  true
;

: %jsonrpc-parse-header-field ( string jsonrpc -- ok? )
  {: jsonrpc :}
  %split-field-name 2swap
  2dup s" Content-Length" string-ci= if
    2drop jsonrpc %jsonrpc-parse-content-length exit
  then
  2dup s" Content-Type" string-ci= if 2drop 2drop true exit then
  2drop false
;

: jsonrpc-parse-header-part ( jsonrpc -- u )
  {: jsonrpc :}
  0 jsonrpc %jsonrpc-content-length !
  begin
    pad %/pad jsonrpc %jsonrpc-reader reader-read-line-crlf throw
    abort" eof-in-header nyi"
    dup 0= if drop jsonrpc %jsonrpc-content-length @ exit then
    pad swap jsonrpc %jsonrpc-parse-header-field
    0= abort" invalid header-field"
  again
;

: %jsonrpc-read-content ( u jsonrpc -- c-addr u )
  {: jsonrpc :}
  dup jsonrpc %jsonrpc-request-region region-alloc swap ( c-addr u )
  2dup jsonrpc %jsonrpc-reader reader-read-sloppy throw
;

: %jsonrpc-write ( c-addr u jsonrpc -- )
  {: a u j :}
  a u j %jsonrpc-response-writer writer-write throw
  u <> abort" partial write"
;

: %jsonrpc-write-json ( json jsonrpc -- )
  %jsonrpc-response-writer swap json-write throw
;

: %jsonrpc-header-template s\" Content-Length:        \r\n\r\n" ;

: %jsonrpc-patch-content-length ( c-addr u -- )
  %jsonrpc-header-template nip {: a u hlen :}
  u hlen - s>d <# #s #> hlen 4 - over - a + swap move
;

: %jsonrpc-send-message ( json jsonrpc -- )
  {: json j :}
  j %jsonrpc-request-region j %jsonrpc-response-writer init-region-writer drop
  %jsonrpc-header-template j %jsonrpc-write
  json j %jsonrpc-write-json
  s\" \n" j %jsonrpc-write
  j %jsonrpc-response-writer region-writer-finish
  2dup %jsonrpc-patch-content-length
  tuck j %jsonrpc-writer writer-write throw <> abort" partial write"
  j %jsonrpc-writer writer-flush throw
;

: jsonrpc-builder ( jsonrpc -- jb ) %jsonrpc-parser json-builder ;

: %jsonrpc-make-error ( code msg data jb -- json )
  {: c msg data j :}
  j jb-{
  s" code" c j jb-:
  s" message" msg j jb-:
  data json-null <> if
    s" data" data j jb-:
  then
  j jb-}
;

: jsonrpc-send-error ( id code msg data jsonrpc -- )
  dup jsonrpc-builder {: id code msg data j jb :}
  jb jb-{
  s" jsonrpc" s" 2.0" jb jb-str jb jb-:
  s" id" id jb jb-:
  s" error" code msg data jb %jsonrpc-make-error jb jb-:
  jb jb-}
  j %jsonrpc-send-message
;

: jsonrpc-make-string ( string$ jsonrpc -- json ) jsonrpc-builder jb-str ;

: %jsonrpc-send-parse-error ( jsonrpc -- )
  {: j :}
  json-null
  JSONRPC_PARSE_ERROR json-make-fixnum
  s" parse error" j jsonrpc-make-string
  json-null
  j jsonrpc-send-error
;

: %jsonrpc-send-invalid-request ( request jsonrpc -- )
  {: r j :}
  r json-object? if
    s" id" r json-object-get 0= if drop json-null then
  else
    json-null
  then
  JSONRPC_INVALID_REQUEST json-make-fixnum
  s" invalid request" j jsonrpc-make-string
  json-null
  j jsonrpc-send-error
;

: %jsonrpc-send-method-not-found ( request jsonrpc -- )
  {: r j :}
  s" id" r json-ref
  JSONRPC_METHOD_NOT_FOUND json-make-fixnum
  s" method not found" j jsonrpc-make-string
  json-null
  j jsonrpc-send-error
;

: jsonrpc-send-response ( result request jsonrpc -- )
  dup jsonrpc-builder {: result request j jb :}
  jb jb-{
  s" jsonrpc" s" 2.0" jb jb-str jb jb-:
  s" id" s" id" request json-ref jb jb-:
  s" result" result jb jb-:
  jb jb-}
  j %jsonrpc-send-message
;

: %jsonrpc-version-ok? ( json -- ok? )
  {: json :}
  s" jsonrpc" json json-object-get
  0= if drop false exit then
  dup json-string? 0= if drop false exit then
  json-string-slice s" 2.0" compare 0<> if false exit then
  true
;

: %jsonrpc-id-ok? ( json -- ok? )
  s" id" rot json-object-get 0= if drop true exit then {: id :}
  id json-integer? if true exit then
  id json-string? if true exit then
  json-null =
;

: %jsonrpc-method-ok? ( json -- flag )
  s" method" rot json-object-get if json-string? else drop false then
;

: %jsonrpc-request-valid? ( json -- flag )
  {: json :}
  json json-object? 0= if false exit then
  json %jsonrpc-version-ok? 0= if false exit then
  json %jsonrpc-method-ok? 0= if false exit then
  json %jsonrpc-id-ok?
;

: %jsonrcp-call-method ( request method -- shutdown? )
  dup %method-env @ swap %method-xt @ execute
;

: %jsonrpc-dispatch-method ( name request jsonrpc -- shutdown? )
  {: name r j :}
  name j %jsonrpc-methods hashtable-get if
    r swap %jsonrcp-call-method
  else
    drop
    ." method not found: " name .json ."  " r .json cr
    r j %jsonrpc-send-method-not-found
    false
  then
;

: %jsonrpc-dispatch-notification ( name request jsonrpc -- shutdown? )
  {: name r j :}
  name j %jsonrpc-notifications hashtable-get if
    r swap %jsonrcp-call-method
  else
    drop
    ." notification method not found: " name .json cr
    false
  then
;

: %%jsonrpc-dispatch ( name request jsonrpc -- shutdown? )
  over s" id" rot json-object-get nip if
    %jsonrpc-dispatch-method
  else
    %jsonrpc-dispatch-notification
  then
;

: %jsonrpc-dispatch ( json jsonrpc -- shutdown? )
  {: json j :}
  \ ." %jsonrpc-dispatch: " json .json cr
  json %jsonrpc-request-valid? 0= if
    json j %jsonrpc-send-invalid-request false exit
  then
  s" method" json json-ref json j %%jsonrpc-dispatch
;

: %jsonrpc-parse-json ( c-addr u jsonrpc -- json error? )
  %jsonrpc-parser json-parse 0=
;

: %json-parse-message ( jsonrpc -- json error? )
  {: j :}
  j jsonrpc-parse-header-part
  j %jsonrpc-read-content
  j %jsonrpc-parse-json
;

: jsonrpc-process-request ( jsonrpc -- shutdown? )
  {: j :}
  j %json-parse-message if drop j %jsonrpc-send-parse-error false exit then
  j %jsonrpc-dispatch
  j %jsonrpc-request-region region-free-all
;

: jsonrpc-process-requests ( jsonrpc -- )
  {: j :}
  begin
    j jsonrpc-process-request
  until
;

: %jsonrpc-string-hash ( json -- u ) json-string-slice jhash ;

: %jsonrpc-string= ( json1 json2 -- u )
  json-string-slice rot json-string-slice compare 0=
;

: %jsonrpc-init-methods ( jsonrpc a-addr -- )
  {: j a :}
  ['] %jsonrpc-string-hash ['] %jsonrpc-string= 0
  j %jsonrpc-permanent-region region>allocator
  a init-hashtable drop
;

: jsonrpc-register-method ( c-addr u xt env jsonrpc -- )
  dup %jsonrpc-permanent-region {: xt env j region :}
  region json-make-string xt env %/method region region-alloc %init-method
  dup %method-name @ swap j %jsonrpc-methods hashtable-put
;

: jsonrpc-register-notification ( c-addr u xt env jsonrpc -- )
  dup %jsonrpc-permanent-region {: xt env j region :}
  region json-make-string xt env %/method region region-alloc %init-method
  dup %method-name @ swap j %jsonrpc-notifications hashtable-put
;

: init-jsonrpc ( in-fd out-fd a-addr -- jsonrpc )
  >r
  r@ %jsonrpc-writer init-fd-writer drop
  r@ %jsonrpc-reader init-fd-reader drop
  0 r@ %jsonrpc-content-length !
  r@ %jsonrpc-request-region init-region
  0 swap r@ %jsonrpc-parser init-json-parser drop
  r@ %jsonrpc-permanent-region init-region drop
  r@ r@ %jsonrpc-methods %jsonrpc-init-methods
  r@ r@ %jsonrpc-notifications %jsonrpc-init-methods
  r>
;

: jsonrpc-make-request ( id method params jb -- json )
  {: id method params j :}
  j jb-{
  s" jsonrpc" s" 2.0" j jb-str j jb-:
  s" id" id j jb-:
  s" method"  method j jb-:
  s" params" params j jb-:
  j jb-}
;

: jsonrpc-make-notification ( method params jb -- json )
  {: method params j :}
  j jb-{
  s" jsonrpc" s" 2.0" j jb-str j jb-:
  s" method" method j jb-:
  s" params" params j jb-:
  j jb-}
;

: jsonrpc-send-notification ( method params jsonrpc -- )
  {: j :}
  j jsonrpc-builder jsonrpc-make-notification
  j %jsonrpc-send-message
;
