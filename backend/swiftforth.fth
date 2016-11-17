: 2null   0 0 ;
: big-size   200 100 ;

\ Similar to ACCEPT but doesn't display the received characters.
\ Reads one line (whithout trailing newline).
: accept-no-echo ( addr u1 -- u2 )
  tuck
  begin				( u1 addr u )
    dup 0= if 2drop exit then
    key
    case
      \ remove \n and \r
      10 of nip - exit endof
      13 of endof  \ FIXME: should detect \r\n sequences properly
      2 pick c!
      1 /string
      0
    endcase
  again ;

create winning-personality
  4 cells , 19 , 0 , 0 ,
  ' noop , ' noop , ' noop ,
  'emit @ ,
  'type @ ,
  '?type @ ,
  'cr @ ,
  ' noop , \ page
  ' drop , \ attribute
  'key @ ,
  'key? @ ,
  'ekey @ ,
  'ekey? @ ,
  'akey @ ,
  'pushtext @ ,
  ' 2drop , \ at-xy
  ' 2null , \ get-xy
  ' big-size , \ get-size
  ' accept-no-echo , \ accept

:noname cr ." ok " ; is prompt
: clearstack ( ... -- ) begin depth 0> while drop repeat ;
: repl
  winning-personality open-personality /interpreter
  \ Could just call QUIT but running our own REPL is cooler.
  begin
    state @ 0= if prompt then
    \ NOTE: refill prints a space (don't ask me why)
    refill 0= abort" refill failed"
    source ['] evaluate catch
    ?dup if cr ." Error: " .catch cr
	    clearstack
	 then
  again ;

repl
