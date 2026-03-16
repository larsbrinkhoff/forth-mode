\ utf8.fth --- Encoding and decoding to/from UTF-8.

\ Return the number of bytes needed to encode the code point
\ CODEPOINT.
: utf8-encoded-size ( codepoint -- u )
  dup     $80 u< if drop 1 exit then
  dup    $800 u< if drop 2 exit then
  dup  $10000 u< if drop 3 exit then
  dup $110000 u< if drop 4 exit then
  ." Invalid code point: " u. cr
  true abort" invalid code point"
;

\ Store the code point CODEPOINT as UTF-8 encoded sequence of bytes
\ beginning at address C-ADDR.
: utf8-encode ( codepoint c-addr -- )
  over $80     u< if c! exit then
  over $800    u< if >r
		     dup  6 rshift               %11000000 or r@ c!
		     dup           %00111111 and %10000000 or r> 1+ c!
		     drop exit
		  then
  over $10000  u< if >r
		     dup 12 rshift               %11100000 or r@ c!
		     dup  6 rshift %00111111 and %10000000 or r@ 1+ c!
		     dup           %00111111 and %10000000 or r> 2 + c!
		     drop exit
		  then
  over $110000 u< if >r
		     dup 18 rshift               %11110000 or r@ c!
		     dup 12 rshift %00111111 and %10000000 or r@ 1+ c!
		     dup  6 rshift %00111111 and %10000000 or r@ 2 + c!
		     dup           %00111111 and %10000000 or r> 3 + c!
		     drop exit
		  then
  ." Invalid code point: " u. cr
  true abort" invalid code point"
;
