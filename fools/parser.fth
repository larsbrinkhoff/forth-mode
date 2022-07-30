\ parser.fth --- Parser for Forth source code.
\
\ It's really more of a tokenizer that can be customized with a
\ wordlist.

require ascii.fth
require charset.fth
require buffer.fth
require reader.fth

0
1 cells  +field %parser-reader
/buffer  +field %parser-token-buffer
32 cells +field %parser-token-mem
1 cells  +field %parser-line
1 cells  +field %parser-column
1 cells  +field %parser-wordlist
1 cells  +field %parser-env
constant /parser

: %parser-update-location ( char parser -- )
  {: p :}
  'LF' = if
    1 p %parser-line +!
    0 p %parser-column !
  else
    1 p %parser-column +!
  then
;

: %init-whitespace-chars ( charset -- charset )
  >r
  'SPACE' r@ charset-add
  'HTAB' r@ charset-add
  'CR' r@ charset-add
  'LF' r@ charset-add
  'FF' r@ charset-add
  r>
;

create %parser-whitespace-chars ' %init-whitespace-chars charset,

: %parser-skip-whitespace? ( parser char -- parser flag )
  dup %parser-whitespace-chars charset-member? if
    over %parser-update-location
    true
  else
    drop
    false
  then
;

: %parser-skip-non-whitespace? ( parser char -- parser flag )
  dup %parser-whitespace-chars charset-member? if
    drop
    false
  else
    2dup swap %parser-update-location
    over %parser-token-buffer
    \ silently truncate overly long tokens
    dup buffer-remaining 0= if 2drop else buffer-put-byte then
    true
  then
;

: %parser-scan ( parser -- c-addr u )
  {: p :}
  p ['] %parser-skip-whitespace?     p %parser-reader @ reader-skip drop
  p %parser-token-buffer buffer-clear
  p ['] %parser-skip-non-whitespace? p %parser-reader @ reader-skip drop
  p %parser-token-buffer buffer-flip
  p %parser-token-buffer buffer-slice
;

: %parser-dispatch ( c-addr u parser -- )
  {: p :}
  p %parser-wordlist @ search-wordlist
  0= if exit then
  p %parser-env @ swap execute
;

: %parser-skip ( xt[char -- flag] parser char -- xt parser flag )
  {: xt p c :}
  c xt execute if
    c p %parser-update-location
    xt p true
  else
    xt p false
  then
;

: parser-skip ( xt[char -- flag] parser -- )
  ['] %parser-skip over %parser-reader @ reader-skip 2drop
;

: parser-parse ( reader parser -- )
  {: p :}
  p %parser-reader !
  0 p %parser-line !
  0 p %parser-column !
  begin
    p %parser-scan
    dup 0= if 2drop exit then
    p %parser-dispatch
  again
;

: %parser-word-at ( col parser -- word$ )
  dup %parser-line @ {: col p line :}
  begin
    p %parser-line @ line u> if 0 0  exit then
    p %parser-column @ col u> if p %parser-token-buffer buffer-slice exit then
    p %parser-scan s" " compare 0= if 0 0 exit then
  again
;

: parser-word-at ( line col reader parser -- word$ )
  tuck %parser-reader !
  {: line col p :}
  0 p %parser-line !
  0 p %parser-column !
  begin
    p %parser-line @ line = if col p %parser-word-at exit then
    p %parser-line @ line u> if 0 0 exit then
    p %parser-scan s" " compare 0= if 0 0 exit then
  again
;

: %parser-token-mem-size ( -- u ) 0 %parser-line 0 %parser-token-mem - ;

: init-parser ( wordlist env a-addr -- parser )
  >r
  r@ %parser-env !
  r@ %parser-wordlist !
  r@ %parser-token-mem %parser-token-mem-size
  r@ %parser-token-buffer init-buffer drop
  0 r@ %parser-reader !
  0 r@ %parser-line !
  0 r@ %parser-column !
  r>
;

: deinit-parser ( parser -- ) drop ;

: make-parser ( worlist env a-addr -- parser )
  0 0 /parser allocate throw init-parser
;

: drop-parser ( parser -- ) dup deinit-parser free throw ;
