\ Record state changes when words are processed by the text interpreter.

create buf  256 cells allot
variable 'buf
: 0buf   buf 'buf ! ;
: >buf   'buf @ !  cell 'buf +! ;
: .buf   'buf @ buf ?do i @ . cell +loop ;

defer old-parsed   action-of parsed is old-parsed
: source-   source drop - ;
: introspect   over source- >buf  depth 2 - >buf  old-parsed ;

: !parsed   ['] introspect is parsed ;
: 0parsed   action-of old-parsed is parsed ;

: start-introspection   0buf !parsed ;
: stop-introspection   cr .buf 0parsed ;
