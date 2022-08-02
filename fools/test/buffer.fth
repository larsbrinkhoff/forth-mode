require prelude.fth
require buffer.fth
require tap.fth

1024 constant slice-len
create slice slice-len allot
create buf /buffer allot

: test1 ( -- )
  slice slice-len buf init-buffer buf = ok
  buf buffer-remaining 0= ok
  buf buffer-start 0= ok
  buf buffer-end 0= ok

  buf buffer-clear
  buf buffer-start 0= ok
  buf buffer-end 1024 = ok

  s" abc" buf buffer-put-slice
  buf buffer-start 3 = ok
  buf buffer-end 1024 = ok

  buf buffer-flip
  buf buffer-start 0 = ok
  buf buffer-end 3 = ok

  s" xyz" pad swap move
  pad 3 s" xyz" compare 0= ok

  pad 3 buf buffer-get-slice
  pad 3 s" abc" compare 0= ok
  buf buffer-start 3 = ok
  buf buffer-end 3 = ok

  buf buffer-clear
  buf buffer-start 0 = ok
  buf buffer-end 1024 = ok
  s" def" buf buffer-put-slice
  buf buffer-start 3 = ok
  buf buffer-end 1024 = ok

  buf buffer-flip
  buf buffer-start 0 = ok
  buf buffer-end 3 = ok

  buf buffer-get-byte 'd' = ok
  buf buffer-get-byte 'e' = ok
  buf buffer-get-byte 'f' = ok
  buf buffer-start 3 = ok
  buf buffer-end 3 = ok
;

: test-put-byte ( -- )
  slice slice-len buf init-buffer buf = ok
  buf buffer-clear
  'a' buf buffer-put-byte
  'b' buf buffer-put-byte
  'c' buf buffer-put-byte
  buf buffer-flip
  buf buffer-slice s" abc" compare 0= ok

  slice slice-len -1 fill
  slice 8 buf init-buffer buf = ok
  buf buffer-clear
  s" 12345678"
  begin dup 0<> while
    over c@ buf buffer-put-byte
    1 /string
  repeat
  2drop
  buf buffer-flip
  buf buffer-slice s" 12345678" compare 0= ok
  slice 8 s" 12345678" compare 0= ok
  slice 8 + @ -1 = ok
;

: test-looking-at? ( -- )
  slice slice-len buf init-buffer buf = ok
  buf buffer-clear
  s" abc" buf buffer-put-slice
  buf buffer-flip
  s" abc" buf buffer-looking-at? 0= ok 0= ok
  s" ab" buf buffer-looking-at? 0= ok 0= ok
  s" " buf buffer-looking-at? 0= ok 0= ok
  s" abcd" buf buffer-looking-at? ok 1 = ok
  s" abcdef" buf buffer-looking-at? ok 3 = ok
  s" abde" buf buffer-looking-at? 0= ok 2 = ok
  s" d" buf buffer-looking-at? 0= ok 1 = ok
;

: main ( -- )
  test1
  test-put-byte
  test-looking-at?
;

' main 47 run-tests
