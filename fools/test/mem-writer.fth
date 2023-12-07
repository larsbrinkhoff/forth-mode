
require prelude.fth
require mem-writer.fth
require tap.fth

create mem 100 allot
create memwriter /mem-writer allot

: test1 ( -- )
  mem 100 memwriter init-mem-writer drop
  s" abc" memwriter writer-write 0= ok 3 = ok
  s" 123" memwriter writer-write 0= ok 3 = ok
  memwriter mem-writer-slice s" abc123" compare 0= ok
  mem 6 s" abc123" compare 0= ok
;

: test-flush ( -- )
  mem 100 memwriter init-mem-writer drop
  s" abc" memwriter writer-write 0= ok 3 = ok
  memwriter mem-writer-slice s" abc" compare 0= ok
  memwriter writer-flush 0 <> ok
  memwriter mem-writer-slice s" abc" compare 0= ok
;

: test-write-byte ( -- )
  mem 8 memwriter init-mem-writer drop
  mem 100 -1 fill
  mem 0 + @ -1 = ok
  mem 8 + @ -1 = ok
  s" 12345678"
  begin dup 0<> while
    over c@ memwriter writer-write-byte throw
    1 /string
  repeat
  2drop
  mem 8 s" 12345678" compare 0= ok
  mem 8 + @ -1 = ok
  mem 16 + @ -1 = ok
;

: main ( -- )
  test1
  test-flush
  test-write-byte
;

' main 17 run-tests
