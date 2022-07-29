
require prelude.fth
require writer.fth
require tap.fth

100 constant myblock-len
variable mypos
create myblock myblock-len allot
create mywriter /writer allot
1024 constant mybuffer-len
create mybuffer mybuffer-len allot

: mywrite ( c-addr u env -- u ior )
  dup @ {: a u pos& pos :}
  pos u + myblock-len u> 0= if
    a myblock pos + u move
    pos u + pos& !
    u 0
  else
    a myblock pos + myblock-len pos - move
    myblock-len pos& !
    myblock-len pos - $bad
  then
;

: test-init ( -- )
  0 mypos !
  mypos ['] mywrite mybuffer mybuffer-len mywriter init-writer-with-buffer
  mywriter = ok

  \ mypos ['] mywrite mywriter init-writer mywriter = ok
;

: test-write ( -- )
  0 mypos !
  mypos ['] mywrite mybuffer 10 mywriter init-writer-with-buffer
  drop

  mypos @ 0 = ok
  s" abc" mywriter writer-write 0= ok 3 = ok
  mypos @ 0 = ok
  mywriter %writer-buffer buffer-start 3 = ok

  s" " mywriter writer-write 0= ok 0 = ok
  mypos @ 0 = ok
  mywriter %writer-buffer buffer-start 3 = ok
;

: test-flush ( -- )
  0 mypos !
  mypos ['] mywrite mybuffer mybuffer-len mywriter init-writer-with-buffer
  drop

  mypos @ 0 = ok
  s" 1234567" mywriter writer-write 0= ok 7 = ok
  mypos @ 0 = ok
  mywriter %writer-buffer buffer-start 7 = ok

  mywriter writer-flush 0= ok
  mypos @ 7 = ok
  mywriter %writer-buffer buffer-start 0 = ok

  mypos @ 7 = ok
  s" 89" mywriter writer-write 0= ok 2 = ok
  mypos @ 7 = ok
  mywriter %writer-buffer buffer-start 2 = ok

  mywriter writer-flush 0= ok
  mypos @ 9 = ok
  mywriter %writer-buffer buffer-start 0 = ok

  myblock 9 s" 123456789" compare 0= ok
;

: test-write-long ( -- )
  0 mypos !
  mypos ['] mywrite mybuffer 10 mywriter init-writer-with-buffer
  drop

  s" abcdefghijklmnopqrstuvwxyz" mywriter writer-write 0= ok 26 = ok
  mypos @ 26 = ok
  mywriter %writer-buffer buffer-start 0 = ok

  s" 1234" mywriter writer-write 0= ok 4 = ok
  mypos @ 26 = ok
  mywriter %writer-buffer buffer-start 4 = ok
  s" 567890123456789012345" mywriter writer-write 0= ok 21 = ok
  mypos @ 51 = ok
  mywriter %writer-buffer buffer-start 0 = ok

  myblock 51 s" abcdefghijklmnopqrstuvwxyz1234567890123456789012345"
  compare 0= ok
;

: test-write-byte ( -- )
  0 mypos !
  mypos ['] mywrite mybuffer 10 mywriter init-writer-with-buffer
  drop

  '1' mywriter writer-write-byte 0= ok
  '2' mywriter writer-write-byte 0= ok
  '3' mywriter writer-write-byte 0= ok
  mypos @ 0 = ok
  mywriter %writer-buffer buffer-start 3 = ok
  mywriter writer-flush 0= ok
  mypos @ 3 = ok
  myblock 3 s" 123" compare 0= ok

  0 mypos !
  mypos ['] mywrite mybuffer 10 mywriter init-writer-with-buffer
  drop
  15 0 ?do
    i 10 u< if '0' else 'A' 10 - then i + mywriter writer-write-byte 0= ok
  loop
  mypos @ 10 = ok
  mywriter %writer-buffer buffer-start 5 = ok
  mywriter writer-flush 0= ok
  mypos @ 15 = ok
  myblock 15 s" 0123456789ABCDE" compare 0= ok
;

: main ( -- )
  test-init
  test-write
  test-flush
  test-write-long
  test-write-byte
;

' main 69 run-tests
