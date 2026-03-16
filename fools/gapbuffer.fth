\ gapbuffer.fth --- A buffer for text with a gap.
\
\ Like Emacs's buffers, but without markers, textprops and almost
\ anything else.
\
\ Naming conventions in this file:
\  - Offset: a number of bytes
\  - Index: a 0-based "character position"
\  - Position: a line/column pair.
\
\ This should eventually treat "characters" as code points, but for
\ now characters are just bytes.

require misc.fth
require assert.fth
require ascii.fth

begin-structure /gapbuffer
  field: %gapbuffer-base	\ pointer to the start of the text
  field: %gapbuffer-capacity	\ number of allocated bytes
  field: %gapbuffer-gap-start	\ offset where the gap starts
  field: %gapbuffer-gap-end	\ offset where the gap ends
end-structure

1024 constant %gapbuffer-default-gap-length

: init-gapbuffer ( capacity a-addr -- gapbuffer )
  >r
  dup r@ %gapbuffer-capacity !
  dup r@ %gapbuffer-gap-end !
  allocate throw r@ %gapbuffer-base !
  0 r@ %gapbuffer-gap-start !
  r>
;

: deinit-gapbuffer ( gapbuffer -- ) %gapbuffer-base @ free throw ;

: %gapbuffer-gap ( gapbuffer -- start end )
  dup %gapbuffer-gap-start @ swap %gapbuffer-gap-end @
;

: %gapbuffer-gap-length ( gapbuffer -- u )
  dup %gapbuffer-gap-end @ swap %gapbuffer-gap-start @  -
;

: %gapbuffer-length ( gapbuffer -- )
  dup %gapbuffer-capacity @ swap %gapbuffer-gap-length -
;

: %gapbuffer-gap-base ( gapbuffer -- c-addr )
  dup %gapbuffer-base @ swap %gapbuffer-gap-start @ +
;

\ Move the start of the gap to the index INDEX.
: %gapbuffer-move-gap ( index gapbuffer -- )
  >r r@ %gapbuffer-gap-length over +
  r@ %gapbuffer-gap-start @ r@ %gapbuffer-gap-end @
  r@ %gapbuffer-base @ r>
  {: t f s e base b :}
  s t = if exit then
  t s u< if
    base t + base f + s t - move
  else
    base e + base s + t s - move
  then
  t b %gapbuffer-gap-start !
  f b %gapbuffer-gap-end !
;

: %gapbuffer-grow ( new-gap-len gapbuffer -- )
  >r r@ %gapbuffer-gap-length - r@ %gapbuffer-gap-end @
  r@ %gapbuffer-capacity @ r>  {: diff end capacity b :}
  b %gapbuffer-base @ capacity diff + resize throw ( new-base )
  dup b %gapbuffer-base !				       ( new-base )
  end + dup diff + capacity end - move
  diff b %gapbuffer-capacity +!
  diff b %gapbuffer-gap-end +!
;

\ Choose a new size for the gap.  It should be at least LEN.
: %gapbuffer-new-gap-length ( len gapbuffer -- u )
  drop {: l :}
  l 2* %gapbuffer-default-gap-length max
;

: %gapbuffer-ensure-gap ( len index gapbuffer -- )
  {: l o b :}
  o b %gapbuffer-move-gap
  b %gapbuffer-gap-length l u< if
    l b %gapbuffer-new-gap-length b %gapbuffer-grow
  then
;

: gapbuffer-insert ( c-addr u index gapbuffer -- )
  assert( 2dup %gapbuffer-length u> 0= )
  {: a u o b :}
  u o b %gapbuffer-ensure-gap
  a b %gapbuffer-gap-base u move
  u b %gapbuffer-gap-start +!
;

: %gapbuffer-copy ( gapbuffer -- c-addr u )
  >r r@ %gapbuffer-length dup allocate throw
  r@ %gapbuffer-base @ r@ %gapbuffer-capacity @
  r@ %gapbuffer-gap-start @ r@ %gapbuffer-gap-end @ r>
  {: len a base capacity s e b :}
  base a s move
  base e + a s + capacity e - move
  a len
;

\ Move as many bytes as possible from C-ADDR1/U1 to C-ADDR2/U2.  The
\ result C-ADDR3/U3 is the unused of part C-ADDR2/U2.
: %move-part ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 )
  {: a1 u1 a2 u2 :}
  u1 u2 u< if
    a1 a2 u1 move
    a2 u2 u1 /string
  else
    a1 a2 u2 move
    a2 0
  then
;

: gapbuffer-read ( c-addr u index gapbuffer -- u2 )
  assert( 2dup %gapbuffer-length u> 0= )
  >r r@ %gapbuffer-base @ r@ %gapbuffer-capacity @
  r@ %gapbuffer-gap r>
  {: a u o base capacity start end b :}
  a u
  o start min              dup base +    start rot - 2swap %move-part
  o end start - + end max  dup base + capacity rot - 2swap %move-part
  ( a' u' )
  nip u swap -
;

: %gapbuffer-index>offset ( index gapbuffer -- offset )
  {: b :}
  dup b %gapbuffer-gap-start @ u< 0= if b %gapbuffer-gap-length + then
;

: %gapbuffer-byte-at ( index gapbuffer -- char )
  {: b :}
  b %gapbuffer-index>offset b %gapbuffer-base @ + c@
;

: %gapbuffer-skip ( xt[char -- flag] index gapbuffer -- index2 )
  assert( 2dup %gapbuffer-length u> 0= )
  {: xt index b :}
  begin
    index b %gapbuffer-length = if index exit then
    index b %gapbuffer-byte-at xt execute 0= if index exit then
    index 1+ to index
  again
;

: %%gapbuffer-lines+ ( lines char -- lines' flag )
  over 0= if drop false exit then
  'LF' = if 1- true exit then
  true
;

\ Move LINES lines forward from index START.  Return the index END
\ either when the requested lines were skipped of when reaching
\ end-of-buffer. LINES2 is the number of lines that could not be
\ skipped at end-of-buffer.
: %gapbuffer-lines+ ( lines start gapbuffer -- lines2 end )
  ['] %%gapbuffer-lines+ rot rot %gapbuffer-skip
;

: %%gapbuffer-cols+ ( cols char -- cols' flag )
  'LF' = if false exit then
  dup 0= if false exit then
  1- true
;

\ FIXME: implement the utf-16 part.
\ Move COLS utf-16 code units forward from index START.  Return the index
\ END at the end.
: %gapbuffer-cols+ ( cols start gapbuffer -- cols2 end )
  ['] %%gapbuffer-cols+ rot rot %gapbuffer-skip
;

: gapbuffer-position>index ( line col gapbuffer -- index out-of-bounds? )
  {: b :}
  swap 0 b %gapbuffer-lines+ swap 0<> if nip true exit then
  b %gapbuffer-cols+ swap 0<>
;

: gapbuffer-delete ( start end gapbuffer -- )
  {: s e b :}
  assert( s b %gapbuffer-length u> 0= )
  assert( e b %gapbuffer-length u> 0= )
  assert( s e u> 0= )
  s b %gapbuffer-move-gap
  e s - b %gapbuffer-gap-end +!
;

: gapbuffer-change ( start end text$ gapbuffer -- )
  {: start end t tlen b :}
  start end b gapbuffer-delete
  t tlen start b gapbuffer-insert
;

: %gapbuffer-insert-file ( fid index gapbuffer -- )
  rot dup file-size throw drop {: o b f size :}
  size o b %gapbuffer-ensure-gap
  b %gapbuffer-gap-base size f read-file throw
  size <> abort" short read"
  size b %gapbuffer-gap-start +!
;

: make-gapbuffer-from-file ( c-addr u -- gapbuffer )
  r/o open-file throw
  /gapbuffer allocate throw {: file b :}
  0 b init-gapbuffer drop
  file 0 b %gapbuffer-insert-file
  file close-file throw
  b
;
