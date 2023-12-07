
\ POS is the first position in STRING$ where XT returns true.
\ FOUND? is false only if XT always returned false.
: string-position ( xt[char -- flag] string$ -- pos found? )
  rot rot {: xt a :}
  0 ?do
    a i chars + c@ xt execute if i true unloop exit then
  loop
  -1 false
;

\ Split STRING$ at the first position where XT returns true.
\ If XT never returns true, then STRING2$ is empty.
: string-split ( xt[char -- flag] string$ -- string1$ string2$ )
  2dup 2>r
  string-position 0= if drop r@ then
  2r@ drop swap dup 2r> rot /string
;

\ Return true if PREFIX$ is a prefix of STRING$.
: string-prefix? ( prefix$ string$ -- flag )
  {: pa pl sa sl :}
  sl pl u< if false exit then
  pa pl sa pl compare 0=
;

\ Return true if SUFFIX$ is a suffix of STRING$.
: string-suffix? ( string$ suffix$ -- flag )
  {: sa sl sua sul :}
  sl sul u< if false exit then
  sua sul sa sl sul - + sul compare 0=
;
