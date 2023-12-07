require prelude.fth
require json-parser.fth
require tap.fth
require mem-writer.fth

create parser /json-parser allot
create myregion /region allot

: set-input ( c-addr u -- )
  make-mem-reader myregion init-region parser init-json-parser drop
;

: test-eot ( -- )
  s" " set-input
  parser %json-next-token %jtk-eot = ok 0= ok

  parser %json-next-token %jtk-eot = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s\"  \n\r\n\t " set-input
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-{ ( -- )
  s" {" set-input
  parser %json-next-token %jtk-{ = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"   {" set-input
  parser %json-next-token %jtk-{ = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-} ( -- )
  s" }" set-input
  parser %json-next-token %jtk-} = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"   }" set-input
  parser %json-next-token %jtk-} = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"   { }" set-input
  parser %json-next-token %jtk-{ = ok 0= ok
  parser %json-next-token %jtk-} = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-[ ( -- )
  s"   [ ]" set-input
  parser %json-next-token %jtk-[ = ok 0= ok
  parser %json-next-token %jtk-] = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"   [ { }] ]" set-input
  parser %json-next-token %jtk-[ = ok 0= ok
  parser %json-next-token %jtk-{ = ok 0= ok
  parser %json-next-token %jtk-} = ok 0= ok
  parser %json-next-token %jtk-] = ok 0= ok
  parser %json-next-token %jtk-] = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-: ( -- )
  s"   { : }" set-input
  parser %json-next-token %jtk-{ = ok 0= ok
  parser %json-next-token %jtk-: = ok 0= ok
  parser %json-next-token %jtk-} = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-, ( -- )
  s"   [ ,] " set-input
  parser %json-next-token %jtk-[ = ok 0= ok
  parser %json-next-token %jtk-, = ok 0= ok
  parser %json-next-token %jtk-] = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-match-string ( -- )
  s" null" set-input
  s" null" parser %json-match-string true = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" nul" set-input
  s" nu" parser %json-match-string true = ok
  s" null" parser %json-match-string false = ok
;

: test-true ( -- )
  s" true" set-input
  parser %json-next-token %jtk-true = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" truetrue" set-input
  parser %json-next-token %jtk-true = ok 0= ok
  parser %json-next-token %jtk-true = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" truefalsenull" set-input
  parser %json-next-token %jtk-true = ok 0= ok
  parser %json-next-token %jtk-false = ok 0= ok
  parser %json-next-token %jtk-null = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-int ( -- )
  s" 123" set-input
  parser %json-next-token %jtk-int = ok 123 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"  456  " set-input
  parser %json-next-token %jtk-int = ok 456 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"  789true" set-input
  parser %json-next-token %jtk-int = ok 789 = ok
  parser %json-next-token %jtk-true = ok 0= ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"  0" set-input
  parser %json-next-token %jtk-int = ok 0 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s"  0123" set-input
  parser %json-next-token %jtk-int = ok 0 = ok
  parser %json-next-token %jtk-int = ok 123 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

;

: test-minus ( -- )
  s" -123" set-input
  parser %json-next-token %jtk-int = ok -123 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" -4" set-input
  parser %json-next-token %jtk-int = ok -4 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" - " set-input
  parser %json-next-token %jtk-error = ok 0 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" -0" set-input
  parser %json-next-token %jtk-int = ok 0 = ok
  parser %json-next-token %jtk-eot = ok 0= ok

  s" -0123" set-input
  parser %json-next-token %jtk-int = ok 0 = ok
  parser %json-next-token %jtk-int = ok 123 = ok
  parser %json-next-token %jtk-eot = ok 0= ok
;

: test-string ( -- )
  s\" \"\"" set-input
  parser %json-next-token %jtk-string = ok
  dup json-string? ok
  json-string-slice s" " compare 0= ok

  s\" \"abc\"" set-input
  parser %json-next-token %jtk-string = ok
  dup json-string? ok
  json-string-slice s" abc" compare 0= ok

  s\" \"ab" set-input
  parser %json-next-token %jtk-error = ok
  0 = ok

  s\" \"ab cd\"" set-input
  parser %json-next-token %jtk-string = ok
  json-string-slice s" ab cd" compare 0= ok

  s\" \" \\\\ \"" set-input
  parser %json-next-token %jtk-string = ok
  json-string-slice s"  \ " compare 0= ok

  s\" \" \\\" \\\\ \\b \\f \\n \\r \\t \"" set-input
  parser %json-next-token %jtk-string = ok
  json-string-slice s\"  \" \\ \b \f \n \r \t " compare 0= ok

  s\" \" \\a \"" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\u005C \"" set-input
  parser %json-next-token %jtk-string = ok
  json-string-slice s"  \ " compare 0= ok

  s\" \" \\u00BE \\u20ac \\ud835\\udcac \"" set-input
  parser %json-next-token %jtk-string = ok
  json-string-slice s"  ¾ € 𝒬 " compare 0= ok

  s\" \" \\ud835\\udcai \"" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud835\\Udcac \"" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud835\\ud9ac \"" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud835\\udca" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud835\\u" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud835\\" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud835" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud83" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \\ud83 \"" set-input
  parser %json-next-token %jtk-error = ok drop

  s\" \" \x1b \"" set-input
  parser %json-next-token %jtk-error = ok drop
;

: test-parse-string ( -- )
  s\" \"xyz\"" set-input
  parser %json-parse ok
  json-string-slice s" xyz" compare 0= ok

  s\"   \"abc\"  " set-input
  parser %json-parse ok
  json-string-slice s" abc" compare 0= ok

  s\"   \"123  " set-input
  parser %json-parse 0= ok
  0= ok
;

: test-parse-int ( -- )
  s" 123" set-input
  parser %json-parse ok
  json-integer-value 123 = ok

  s" 0" set-input
  parser %json-parse ok
  json-integer-value 0 = ok

  s" -3456" set-input
  parser %json-parse ok
  json-integer-value -3456 = ok

  s" 04" set-input
  parser %json-parse 0= ok
  0 = ok
;

: test-parse-true ( -- )
  s" true" set-input parser %json-parse ok
  json-true? ok
  s" truefalse" set-input parser %json-parse 0= ok
  0= ok
  s" true," set-input parser %json-parse 0= ok
  0= ok
;

: test-parse-false ( -- )
  s" false" set-input parser %json-parse ok
  json-false? ok
;

: test-parse-null ( -- )
  s" null" set-input parser %json-parse ok
  json-null? ok
;

: test-parse-array ( -- )
  s" []" set-input parser %json-parse ok
  dup json-array? ok
  json-array-length 0= ok

  s" [true]" set-input parser %json-parse ok
  dup json-array? ok
  dup json-array-length 1 = ok
  0 over json-array-get json-true? ok
  drop

  s" [true,false]" set-input parser %json-parse ok
  dup json-array? ok
  dup json-array-length 2 = ok
  0 over json-array-get json-true? ok
  1 over json-array-get json-false? ok
  drop

  s" [truefalse]" set-input parser %json-parse 0= ok
  0= ok

  s" []]" set-input parser %json-parse 0= ok
  0= ok

  s" [[]]" set-input parser %json-parse ok
  dup json-array? ok
  dup json-array-length 1 = ok
  0 swap json-array-get
  dup json-array? ok
  json-array-length 0 = ok

  s" [[]][]" set-input parser %json-parse 0= ok drop

  s" [[],[]]" set-input parser %json-parse ok
  dup json-array? ok
  dup json-array-length 2 = ok
  0 over json-array-get
  dup json-array? ok
  json-array-length 0 = ok
  1 swap json-array-get
  dup json-array? ok
  json-array-length 0 = ok

  s" [[[]]]" set-input parser %json-parse ok
  dup json-array? ok
  dup json-array-length 1 = ok
  0 swap json-array-get
  dup json-array? ok
  dup json-array-length 1 = ok
  0 swap json-array-get
  dup json-array? ok
  json-array-length 0 = ok

  s\" [\"abc\",123,null,false]" set-input parser %json-parse ok
  dup json-array? ok
  0 over json-array-get json-string-slice s" abc" compare 0= ok
  1 over json-array-get json-integer-value 123 = ok
  2 over json-array-get json-null? ok
  3 over json-array-get json-false? ok
  json-array-length 4 = ok

  s" [123,]" set-input parser %json-parse 0= ok drop
;

: test-parse-object ( -- )
  s" {}" set-input parser %json-parse ok
  dup json-object? ok
  json-object-count 0= ok

  s\" {\"abc\":true}" set-input parser %json-parse ok
  dup json-object? ok
  json-object-count 1 = ok

  s\" {\"k\":{}}" set-input parser %json-parse ok
  dup json-object? ok
  json-object-count 1 = ok

  s\" {\"a\":1, \"b\":  2}" set-input parser %json-parse ok
  >r
  r@ json-object? ok
  r@ json-object-count 2 = ok
  s" a" r@ json-object-get ok json-integer-value 1 = ok
  s" b" r@ json-object-get ok json-integer-value 2 = ok
  s" c" r@ json-object-get 0= ok 0= ok
  r> drop

  s\" {123:456}" set-input parser %json-parse 0= ok drop
  s\" {\"a\":456,}" set-input parser %json-parse 0= ok drop
  s\" {123:}" set-input parser %json-parse 0= ok drop
  s\" {\"a\":}" set-input parser %json-parse 0= ok drop
;

: test-json-parse-string ( -- )
  myregion init-region drop
  s\" { \"jsonrpc\": \"2.0\", \"id\": 1 }" myregion json-parse-string
  ok
  json-object? ok
;


: main ( -- )
  test-eot
  test-{
  test-}
  test-[
  test-:
  test-,
  test-match-string
  test-true
  test-int
  test-minus
  test-string
  test-parse-string
  test-parse-int
  test-parse-true
  test-parse-false
  test-parse-null
  test-parse-array
  test-parse-object
  test-json-parse-string
;

' main 255 run-tests
