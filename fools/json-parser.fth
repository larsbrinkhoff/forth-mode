
require json.fth
require json-builder.fth
require reader.fth
require charset.fth
require mem-reader.fth
require utf8.fth

\ Enumeration "json token kind".

0
%+enum constant %jtk-[
%+enum constant %jtk-]
%+enum constant %jtk-{
%+enum constant %jtk-}
%+enum constant %jtk-:
%+enum constant %jtk-,
%+enum constant %jtk-false
%+enum constant %jtk-true
%+enum constant %jtk-null
%+enum constant %jtk-int	\ only integers; no floats yet
%+enum constant %jtk-string
%+enum constant %jtk-eot	\ end of text
%+enum constant %jtk-error	\ invalid token
1- constant %jtk-max

begin-structure /json-parser
  1 cells +field %json-parser-reader  \ reader& to read input from
  /jb     +field %json-parser-builder \ builder to allocate JSON values
end-structure

: init-json-parser ( reader region a-addr -- parser )
  >r
  r@ %json-parser-builder init-jb drop
  r@ %json-parser-reader !
  r>
;

: deinit-json-parser ( parser -- ) %json-parser-builder deinit-jb ;

: %make-parser ( reader values -- parser )
  /json-parser allocate throw init-json-parser
;

: %drop-parser ( parser -- ) dup deinit-json-parser free throw ;

: %json-match-string ( string parser -- flag )
  %json-parser-reader @ {: r :}
  begin		      ( c-addr u )
    dup 0= if 2drop true exit then
    r reader-read-byte		( c-addr u byte ior )
    ?dup if 2drop 2drop false exit then
    >r over c@ r> <> if 2drop false exit then
    1 /string
  again
;

: %init-ws-charset ( charset -- charset )
  >r
  'SPACE' r@ charset-add
  'HTAB' r@ charset-add
  'CR' r@ charset-add
  'LF' r@ charset-add
  r>
;

create %ws-charset ' %init-ws-charset charset,
: %json-ws? ( char -- flag ) %ws-charset charset-member? ;

: %json-skip-ws ( parser -- )
  ['] %json-ws? swap %json-parser-reader @ reader-skip
;

: %json-state-symbol ( string kind parser -- value kind )
  {: kind p :}
  p %json-match-string if
    0 kind
  else
    0 %jtk-error
  then
;

: %digit1-9? ( char -- flag ) '1' '9' 1+ within ;
: %digit>u ( char -- u ) '0' - ;

: %%json-state-int ( minus? int -- value ) swap if negate then ;

: %json-state-int ( minus? int parser -- value )
  dup %json-parser-reader @ {: p r :}
  begin			  ( minus? int )
    r reader-peek-byte	  ( minus? int byte ior )
    ?dup if 2drop %%json-state-int exit then
    dup -1 = if drop %%json-state-int exit then
    dup digit? 0= if drop %%json-state-int exit then
    %digit>u swap 10 * +
    r reader-read-byte throw drop
  again
;

: %json-state-digit1-9 ( digit parser -- value )
  >r %digit>u false swap r> %json-state-int
;

: %json-state-minus ( parser -- value kind )
  dup %json-parser-reader @ {: p r :}
  r reader-read-byte
  ?dup if %jtk-error exit then
  dup '0' = if drop 0 %jtk-int exit then
  dup %digit1-9? if %digit>u true swap p %json-state-int %jtk-int exit then
  drop 0 %jtk-error
;

: %json-state-string-end ( parser -- value kind )
  %json-parser-builder jb-finish-string %jtk-string
;

: %json-parse-hexdigit ( byte -- u ok? )
  dup '0' '9' 1+ within if '0' - true exit then
  dup 'a' 'f' 1+ within if 'a' - 10 + true exit then
  dup 'A' 'F' 1+ within if 'A' - 10 + true exit then
  drop 0 false
;

: %json-parse-4hexdigits ( parser -- u ok? )
  %json-parser-reader @ {: r :}
  0			( u' )
  4 0 ?do
    r reader-read-byte ?dup if 2drop false unloop exit then
    %json-parse-hexdigit 0= if drop false unloop exit then
    swap 4 lshift or
  loop
  true
;

: %json-parse-surrogate-pair ( hi parser -- u ok? )
  {: p :}
  s" \u" p %json-match-string 0= if false exit then
  p %json-parse-4hexdigits 0= if drop false exit then
  dup $DC00 $E000 within 0= if drop false exit then
  ( hi lo )
  $DC00 - swap $D800 - 10 lshift or $10000 + true
;

: %json-state-hexescape ( parser -- ok? )
  {: p :}
  p %json-parse-4hexdigits 0= if drop false exit then
  ( u1 )
  dup $D800 $DC00 within if
    p %json-parse-surrogate-pair 0= if drop false exit then
  then
  ( codepoint )
  p %json-parser-builder jb-add-codepoint
  true
;

: %json-state-escape ( parser -- ok? )
  dup %json-parser-reader @ {: p r :}
  r reader-read-byte ?dup if 2drop false exit then
  case
    '"' of '"' endof
    '\' of '\' endof
    '/' of '/' endof
    'b' of 'BACKSPACE' endof
    'f' of 'FF' endof
    'n' of 'LF' endof
    'r' of 'CR' endof
    't' of 'HTAB' endof
    'u' of p %json-state-hexescape exit endof
    drop false exit
  endcase
  ( codepoint )
  p %json-parser-builder jb-add-codepoint
  true
;

: %json-state-string ( parser -- value kind )
  >r r@ %json-parser-reader @ r@ %json-parser-builder r> {: r jb p :}
  jb jb-begin-string
  begin
    r reader-read-byte ?dup if drop %jtk-error exit then
    case
      -1 of 0 %jtk-error exit endof
      '"' of p %json-state-string-end exit endof
      '\' of p %json-state-escape 0= if 0 %jtk-error exit then endof
      dup $20 u< if %jtk-error exit then
      dup jb jb-add-byte \ FIXME: check invalid utf-8
    endcase
  again
;

: %json-next-token ( parser -- value kind )
  dup %json-parser-reader @ {: p r :}
  p %json-skip-ws
  r reader-read-byte ?dup if drop %jtk-error exit then
  case
    '"' of p %json-state-string endof
    '{' of 0 %jtk-{ endof
    '}' of 0 %jtk-} endof
    '[' of 0 %jtk-[ endof
    ']' of 0 %jtk-] endof
    ':' of 0 %jtk-: endof
    ',' of 0 %jtk-, endof
    'f' of s" alse" %jtk-false p %json-state-symbol endof
    't' of s" rue"  %jtk-true  p %json-state-symbol endof
    'n' of s" ull"  %jtk-null  p %json-state-symbol endof
    -1  of 0 %jtk-eot endof
    '0' of 0 %jtk-int endof
    '-' of p %json-state-minus endof
    dup %digit1-9? if p %json-state-digit1-9 %jtk-int exit then
    %jtk-error exit
  endcase
;

\ Recursive function to parse the first object.  Junk after the first
\ object is not considered an error.
defer %json-parse-1 ( value token parser -- json ok? )

: %json-parse-next-token ( parser -- json ok? )
  dup %json-next-token rot %json-parse-1
;

: %json-parse-array ( parser -- json ok? )
  dup %json-parser-builder  {: p jb :}
  jb jb-begin-array
  p %json-next-token case
    %jtk-] of drop jb jb-finish-array true exit endof
    tuck p %json-parse-1 if swap else nip false exit then
  endcase
  ( json )
  jb jb-push
  begin
    p %json-next-token case
      %jtk-] of drop jb jb-finish-array true exit endof
      %jtk-, of drop p %json-parse-next-token 0= if false exit then endof
      2drop 0 false exit
    endcase
    ( json )
    jb jb-push
  again
;

: %json-parse-member ( value token parser -- key val ok? )
  {: p :}
  case
    %jtk-string of p %json-next-token case
		     %jtk-: of drop p %json-parse-next-token endof
		     2drop drop 0 0 false exit
		   endcase
		endof
    2drop 0 0 false exit
  endcase
;

: %json-parse-object ( parser -- json ok? )
  dup %json-parser-builder  {: p jb :}
  jb jb-begin-object
  p %json-next-token case
    %jtk-} of drop jb jb-finish-object true exit endof
    tuck p %json-parse-member if rot else 2drop drop 0 false exit then
  endcase
  ( key val )
  jb jb-push-member
  begin
    p %json-next-token case
      %jtk-} of drop jb jb-finish-object true exit endof
      %jtk-, of
	drop p %json-next-token p %json-parse-member
	0= if 2drop 0 false exit then
      endof
      2drop 0 false exit
    endcase
    ( key val )
    jb jb-push-member
  again
;

: %%json-parse-1 ( value token parser -- json ok? )
  {: p :}
  case
    %jtk-string of true endof
    %jtk-int of json-make-fixnum true endof
    %jtk-true  of drop json-true  true endof
    %jtk-false of drop json-false true endof
    %jtk-null  of drop json-null  true endof
    %jtk-[ of drop p %json-parse-array endof
    %jtk-{ of drop p %json-parse-object endof
    2drop 0 false exit
  endcase
;

' %%json-parse-1 is %json-parse-1

: %json-parse ( parser -- json ok? )
  {: p :}
  p %json-next-token p %json-parse-1 0= if false exit then
  p %json-next-token nip %jtk-eot = if true else drop 0 false then
;

: json-parse ( c-addr u parser -- json ok? )
  {: p :}
  assert( p %json-parser-reader @ 0= )
  make-mem-reader p %json-parser-reader !

  p %json-parse

  p %json-parser-reader @ drop-mem-reader
  0 p %json-parser-reader !
;

: json-parse-string ( c-addr u region -- json ok? )
  0 swap %make-parser {: p :}
  p json-parse
  p %drop-parser
;

: json-builder ( parser -- jb ) %json-parser-builder ;
