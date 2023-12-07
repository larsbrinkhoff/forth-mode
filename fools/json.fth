\ json.fth --- Data structures to represent JSON values

require bitops.fth
require region.fth

2 constant %json-tag-bits
%11 constant %json-tag-mask
%00 constant %json-fixnum-tag
%01 constant %json-immediate-tag
%10 constant %json-string-tag
%11 constant %json-boxed-tag

: %json-untag ( json -- a-addr ) %json-tag-mask invert and ;
: %json-tag ( x tag -- json ) assert( over %json-tag-mask and 0= ) or ;
: %json-tag-of ( json -- tag ) %json-tag-mask and ;

begin-structure %/json-header
  1 chars    +field %json-header-type
  1 cells 1- +field %json-header-field
end-structure

: %json-header-type@ ( a-addr -- u ) c@ ;
: %json-header-field@ ( a-addr -- u )  @ 8 rshift ;
: %json-header! ( field type a-addr -- u ) >r swap 8 lshift or r> ! ;

begin-structure %/json-string
  %/json-header +
  0 +field %json-string-bytes  \ array of utf-8 code units
end-structure

begin-structure %/json-array
  %/json-header +
  0 +field %json-array-values  \ array of json values
end-structure

begin-structure %/json-object
  %/json-header +
  0 +field %json-object-entries  \ array of key-value pairs
end-structure

: %+enum ( u -- u+1 u ) dup 1+ swap ;

\ Enumeration "json boxed type".
0
%+enum constant %jbt-string
%+enum constant %jbt-array
%+enum constant %jbt-object
1- constant %jbt-max

: json-string? ( json -- flag ) %json-tag-of %json-string-tag = ;

: %json-string-length@ ( a-addr -- u ) %json-header-field@ ;

: json-string-slice ( json -- c-addr u )
  assert( dup json-string? )
  %json-untag dup %json-string-bytes swap %json-string-length@
;

: %json-fixnum? ( json -- flag) %json-tag-of %json-fixnum-tag = ;
: json-integer? ( json -- flag) %json-fixnum? ;

: json-make-fixnum ( n -- json )
  %json-tag-bits lshift %json-fixnum-tag %json-tag
;

: json-integer-value ( json -- n )
  assert( dup %json-fixnum? )
  dup 0< if
    %json-tag-bits rshift
    -1 bits-per-cell %json-tag-bits - lshift or
  else
    %json-tag-bits rshift
  then
;

0
%+enum constant %json-true-imm
%+enum constant %json-false-imm
%+enum constant %json-null-imm
1- constant %json-imm-max

: %json-make-immediate ( imm -- json )
  %json-tag-bits lshift %json-immediate-tag %json-tag
;

: json-true ( -- json )  %json-true-imm  %json-make-immediate ;
: json-false ( -- json ) %json-false-imm %json-make-immediate ;
: json-null ( -- json )  %json-null-imm  %json-make-immediate ;
: json-true? ( json -- flag ) json-true = ;
: json-false? ( json -- flag ) json-false = ;
: json-null? ( json -- flag ) json-null = ;

: json-array? ( json -- flag )
  dup %json-tag-of %json-boxed-tag = if
    %json-untag %json-header-type c@ %jbt-array =
  else
    drop false
  then
;

: json-array-length ( json -- u )
  assert( dup json-array? )
  %json-untag %json-header-field@
;

: json-array-get ( u json -- json2 )
  assert( 2dup json-array-length u< )
  %json-untag %json-array-values swap cells + @
;

: %json-array-slice ( json -- a-addr u )
  assert( dup json-array? )
  %json-untag
  dup %json-array-values
  swap %json-header-field@ cells
;

: json-object? ( json -- flag )
  dup %json-tag-of %json-boxed-tag = if
    %json-untag %json-header-type@ %jbt-object =
  else
    drop false
  then
;

: json-object-count ( json -- u )
  assert( dup json-object? )
  %json-untag %json-header-field@
;

: %json-object-slice ( json -- a-addr u )
  assert( dup json-object? )
  %json-untag
  dup %json-object-entries
  swap %json-header-field@ 2* cells
;

: json-object-get ( c-addr u json -- json2 found? )
  assert( dup json-object? )
  %json-object-slice 2swap {: a u :}
  begin
    dup 0= if 2drop 0 false exit then
    over @ json-string-slice a u compare 0= if drop cell+ @ true exit then
    2 cells /string
  again
;

: json-make-string ( c-addr u region -- json )
  over %/json-header + swap region-alloc >r ( c-addr u ) ( r: a-addr )
  tuck r@ %json-string-bytes swap move	    ( u ) ( r: a-addr )
  %jbt-string r@ %json-header!		    (  ) ( r: a-addr )
  r> %json-string-tag %json-tag
;

: %json-alloc-array ( len region -- a-addr )
  over cells %/json-header + ( len region size )
  swap region-alloc	     ( len a-addr )
  tuck			     ( a-addr len a-addr )
  %jbt-array swap %json-header!
;

: json-make-array ( values len region -- json )
  over swap %json-alloc-array		( values len a-addr )
  >r
  r@ %json-array-values swap cells move
  r> %json-boxed-tag %json-tag
;

: %json-alloc-object ( len region -- a-addr )
  over 2* cells %/json-header + ( len region size )
  swap region-alloc	     ( len a-addr )
  tuck			     ( a-addr len a-addr )
  %jbt-object swap %json-header!
;

: json-make-object ( values len region -- json )
  over swap %json-alloc-object		( values len a-addr )
  >r
  r@ %json-object-entries swap 2* cells move
  r> %json-boxed-tag %json-tag
;
