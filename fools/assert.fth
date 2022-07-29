\ assertions

\ This file is in the public domain. NO WARRANTY.

\    It is a good idea to make your programs self-checking, in
\ particular, if you use an assumption (e.g., that a certain field of a
\ data structure is never zero) that may become wrong during maintenance.
\ Gforth supports assertions for this purpose. They are used like this:

\      assert( FLAG )

\    The code between `assert(' and `)' should compute a flag, that
\ should be true if everything is alright and false otherwise. It should
\ not change anything else on the stack. The overall stack effect of the
\ assertion is `( -- )'. E.g.

\      assert( 1 1 + 2 = ) \ what we learn in school
\      assert( dup 0<> ) \ assert that the top of stack is not zero
\      assert( false ) \ this code should not be reached

\    The need for assertions is different at different times. During
\ debugging, we want more checking, in production we sometimes care more
\ for speed. Therefore, assertions can be turned off, i.e., the assertion
\ becomes a comment. Depending on the importance of an assertion and the
\ time it takes to check it, you may want to turn off some assertions and
\ keep others turned on. Gforth provides several levels of assertions for
\ this purpose:

\ `assert0('       --         gforth       ``assert-zero''
\    important assertions that should always be turned on

\ `assert1('       --         gforth       ``assert-one''
\    normal assertions; turned on by default

\ `assert2('       --         gforth       ``assert-two''
\    debugging assertions

\ `assert3('       --         gforth       ``assert-three''
\    slow assertions that you may not want to turn on in normal debugging;
\ you would turn them on mainly for thorough checking

\ `assert('       --         gforth       ``assert(''
\    equivalent to assert1(

\ `)'       --         gforth       ``close-paren''
\    end an assertion

\    `Assert(' is the same as `assert1('. The variable `assert-level'
\ specifies the highest assertions that are turned on. I.e., at the
\ default `assert-level' of one, `assert0(' and `assert1(' assertions
\ perform checking, while `assert2(' and `assert3(' assertions are
\ treated as comments.

\    Note that the `assert-level' is evaluated at compile-time, not at
\ run-time. I.e., you cannot turn assertions on or off at run-time, you
\ have to set the `assert-level' appropriately before compiling a piece
\ of code. You can compile several pieces of code at several
\ `assert-level's (e.g., a trusted library at level 1 and newly written
\ code at level 3).

\ `assert-level'       -- a-addr         gforth       ``assert-level''
\    all assertions above this level are turned off

\ The program uses the following words
\ from CORE :
\ Variable ! : @ > IF POSTPONE THEN ; immediate 0=
\ from BLOCK-EXT :
\ \
\ from EXCEPTION-EXT :
\ abort"
\ from FILE :
\ (

\ Assertions above this level are turned off.
1 value assert-level ( -- level ) \ gforth

: %assertn ( n -- |msg$ )
  assert-level u>
  if
    postpone (
  else
    >in @ >r ')' parse r> >in !
  then
;

: assert0( ( -- ) 0 %assertn ; immediate
: assert1( ( -- ) 1 %assertn ; immediate
: assert2( ( -- ) 2 %assertn ; immediate
: assert3( ( -- ) 3 %assertn ; immediate
synonym assert( assert1(

: %abort-with-message ( msg$ -- )
  ." Assertion ( " type ." ) " ." failed." cr
  true abort" assertion failed"
;

: ) ( msg$ -- )
  2>r
  postpone 0= postpone if
  2r> postpone sliteral postpone %abort-with-message
  postpone then
; immediate
