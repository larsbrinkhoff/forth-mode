require prelude.fth
require charset.fth
require tap.fth

create charset1 /charset allot

: test1 ( -- )
  charset1 init-charset drop
  'b' charset1 charset-member? false = ok
  'b' charset1 charset-add
  'b' charset1 charset-member? true = ok
  'a' charset1 charset-member? false = ok
  'c' charset1 charset-member? false = ok
;

: test2 ( -- )
  charset1 init-charset drop
  'A' 'Z' charset1 charset-add-range
  'a' 'z' charset1 charset-add-range
  '0' '9' charset1 charset-add-range
  128 0 ?do
    i 'A' 'Z' 1+ within
    i 'a' 'z' 1+ within or
    i '0' '9' 1+ within or if
      i charset1 charset-member? true = ok
    else
      i charset1 charset-member? false = ok
    then
  loop
;

: test3 ( -- )
  charset1 init-charset drop
  s" abefg" charset1 charset-add-string
  'a' charset1 charset-member? true = ok
  'b' charset1 charset-member? true = ok
  'c' charset1 charset-member? false = ok
  'd' charset1 charset-member? false = ok
  'f' charset1 charset-member? true = ok
  'g' charset1 charset-member? true = ok
;

: main ( -- )
  139 test-plan
  depth >r
  test1
  test2
  test3
  depth r> = ok
;

main bye
