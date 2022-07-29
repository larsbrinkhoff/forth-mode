\ ascii.fth --- Constants and operations for ASCII chars.

: 'SPACE' $20 ;
: 'HTAB' $09 ;
: 'CR' $0D ;
: 'LF' $0A ;
: 'FF' $0C ;
: 'BACKSPACE' $08 ;

: digit? ( char -- flag ) '0' '9' 1+ within ;
: downcase ( char1 -- char2 ) dup 'A' 'Z' 1+ within if 'A' - 'a' + then ;

: %char-ci<> ( char1 char2 -- flag ) downcase swap downcase <> ;

\ Case insensitive string comparison.
: string-ci= ( string1$ string2$ -- flag )
  {: a1 u1 a2 u2 :}
  u1 u2 <> if false exit then
  u1 0 ?do
    a1 i + c@ a2 i + c@ %char-ci<> if false unloop exit then
  loop
  true
;
