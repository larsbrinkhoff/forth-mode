require prelude.fth
require json-write.fth
require mem-writer.fth
require tap.fth

create mem 100 allot
create memwriter /mem-writer allot
create myregion /region allot

: check-write ( json expected$ -- )
  2>r
  mem 100 memwriter init-mem-writer drop
  memwriter swap json-write 0= ok
  memwriter mem-writer-slice 2r> compare 0= ok
;

: test-json-write ( -- )
  myregion init-region drop

  json-true  s" true"  check-write
  json-false s" false" check-write
  json-null  s" null"  check-write
  1234 json-make-fixnum s" 1234" check-write
  -321 json-make-fixnum s" -321" check-write
  s" xyz" myregion json-make-string  s\" \"xyz\"" check-write

  here
  s" k1" myregion json-make-string ,
  4 json-make-fixnum ,
  s" k2" myregion json-make-string ,
  5 json-make-fixnum ,
  2 myregion json-make-object  s\" {\"k1\":4, \"k2\":5}" check-write

  here
  1 json-make-fixnum ,
  2 json-make-fixnum ,
  json-true ,
  here 0 myregion json-make-array ,
  here 0 myregion json-make-object ,
  5 myregion json-make-array
  s" [1, 2, true, [], {}]" check-write

  s\"  \n \r \t \f \b \\ \" " myregion json-make-string
  s\" \" \\n \\r \\t \\f \\b \\\\ \\\" \"" check-write

  s\"  \x1b € " myregion json-make-string
  s\" \" \\u001B € \"" check-write

;

: main  ( -- )
  test-json-write
;

' main 21 run-tests
