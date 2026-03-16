\ hash.fth --- Hash functions

require ascii.fth

: jhash ( c-addr u -- u2 )
  {: a u :}
  8388617
  u 0 ?do
    dup 1 lshift
    swap 30 rshift or
    $7fffffff and
    a i + c@ xor
  loop
;

\ case insensitive version
: jhash-ci ( c-addr u -- u2 )
  {: a u :}
  8388617
  u 0 ?do
    dup 1 lshift
    swap 30 rshift or
    $7fffffff and
    a i + c@ downcase xor
  loop
;

: hash-pointer ( x -- u )
  dup 12 lshift +
  dup 22 rshift xor
  dup  4 lshift +
  dup  9 rshift xor
  dup 10 lshift +
  dup  2 rshift xor
  dup  7 lshift +
  dup 12 rshift xor
  [ 1 cells 8 = [if] ]
  dup 44 lshift +
  dup 54 rshift xor
  dup 36 lshift +
  dup 41 rshift xor
  dup 42 lshift +
  dup 34 rshift xor
  dup 39 lshift +
  dup 44 rshift xor
  [ [then] ]
;
