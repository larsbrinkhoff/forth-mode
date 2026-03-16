require prelude.fth
require hash.fth
require tap.fth

: test-jhash ( -- )
  s" foo" jhash $4000161 = ok
  s" +" jhash $1000039 = ok
  s" -" jhash $100003f = ok
  s" rot" jhash $400012a = ok
  s" " jhash $800009 = ok
  s" abcdefghijklmnopqrstuvwxyz" jhash $2443c791 = ok
;

: test-jhash-ci ( -- )
  s" foo" jhash-ci $4000161 = ok
  s" FOO" jhash-ci $4000161 = ok
  s" +" jhash-ci $1000039 = ok
  s" -" jhash-ci $100003f = ok
  s" rot" jhash-ci $400012a = ok
  s" ROT" jhash-ci $400012a = ok
  s" " jhash-ci $800009 = ok
  s" abcdefghijklmnopqrstuvwxyz" jhash-ci $2443c791 = ok
  s" abcdefghijkLMNOPQRSTUVWXYZ" jhash-ci $2443c791 = ok
;

: [32/64] ( u32 u64 -- u )
  parse-name parse-name
  1 cells 4 = if 2drop else 2swap 2drop then
  evaluate
; immediate

: test-hash-pointer ( -- )
  $0 hash-pointer 0 = ok
  $1 hash-pointer [32/64] $af227bb7 $83131092a0823b2d = ok
  $4321 hash-pointer [32/64] $2e7fce30 $777e835269c148ea = ok
  $fedc9876 hash-pointer [32/64] $c01c375b $65d7301114590549  = ok
;

: main ( -- )
  test-jhash
  test-jhash-ci
  test-hash-pointer
;

' main 20 run-tests
