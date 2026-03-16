\ terminal-writer.fth -- Writer API on top of TYPE.

require writer.fth

/writer constant /terminal-writer

: %terminal-write ( c-addr u env -- u2 ior ) drop tuck type 0 ;

: init-terminal-writer ( c-addr u a-addr -- writer )
  >r 0 ['] %terminal-write 2swap r> init-writer-with-buffer
;

4096 constant %terminal-bufsize
create %terminal-buffer %terminal-bufsize allot

create terminal-writer
%terminal-buffer %terminal-bufsize here
/terminal-writer allot init-terminal-writer drop
