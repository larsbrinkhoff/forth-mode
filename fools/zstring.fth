\ zstring.fth --- Helpers for zero terminated strings.

: strlen ( zstring -- u )
  >r 0
  begin
    r@ c@ 0= if r> drop exit then
    r> char+ >r
    1+
  again
;

: zcount ( zstring -- c-addr u ) dup strlen ;

: with-zstring ( xt[ zstring -- ] c-addr u -- )
  dup 1+ allocate throw {: xt a u zstring :}
  a zstring u move
  0 zstring u + c!
  zstring xt catch
  zstring free throw
  throw
;
