\ gforth.fth -- Setup terminal IO for forth-term.el

wordlist constant eterm-wordlist
forth-wordlist eterm-wordlist 2 set-order

26 constant 'C-z'  \ 26 == \032 == Ctrl-z
10 constant '\n'

\ Append the string C-ADDR U to the current command
: cmd, ( c-addr u -- )
  0 ?do
    dup c@ case
      '\n' of '\' c, 'n' c, endof
      '\'  of '\' c, '\' c, endof
      dup c,
    endcase
    char+
  loop
  drop
;

\ Execute XT (which should use CMD, to build the command) and finally
\ send the command as an escape sequence to Emacs.
\
\ The sequences should match the regexp: "\032[^\n]+\n"
: send-emacs-cmd ( xt -- )
  here >r
  'C-z' c,
  execute
  '\n' c,
  r> here over - 2dup type
  nip negate allot
;

\ Enable the status line at the bottom of the window.  This tests our
\ cursor positioning code.
[defined] +status [if] +status [then]

: %ping ( c-addr u -- ) s" pong " cmd, cmd, ;

\ Wordlist for commands that we support
wordlist constant eterm-cmds-wordlist
eterm-cmds-wordlist set-current

\ Simply send C-ADDR U back to Emacs
: ping ( c-addr u -- ) ['] %ping send-emacs-cmd ;

eterm-wordlist set-current

: split-string ( addr u char – addr u1 addr2 u2 )
  >r 2dup      ( addr u addr u  )  ( r: char )
  begin
    dup 0<> if over c@ r@ <> else false then while
    1 /string
  repeat			( addr u addr2 u2 )  ( r: char )
  r> drop
  dup >r
  2swap r> -
  2swap
;

: execute-eterm-command ( c-addr u -- )
  bl split-string
  2swap eterm-cmds-wordlist search-wordlist
  0= abort" invalid eterm command"
  execute
;

\ Apparently they introduced IP-VECTOR after Gforth-0.7.3
[defined] ip-vector [if]

: with-input-vector ( xt vector -- )
  ip-vector @ >r
  ip-vector !
  catch
  r> ip-vector !
  throw
;

: default-key-ior ( -- char|ior ) ['] key-ior default-in with-input-vector ;
: default-key? ( -- flag ) ['] key? default-in with-input-vector ;

[else]

action-of key constant default-key-xt
: default-key-ior ( -- char|ior ) default-key-xt execute ;
: default-key? ( -- flag ) true abort" not yet implemented" ;

: input: ( key-ior-xt key?-xt "name" -- )
  drop create ,
  does> @ dup is key is xkey
;

[then]

: parse-eterm-command ( -- c-addr len )
  here		      ( c-addr' )
  begin
    default-key-ior
    dup 255 u> if throw then \ FIXME: do something more civilized
    dup '\n' <> while
    c,
  repeat
  drop
  here over - save-mem
  dup negate allot
;

\ Filter out commands sent from Emacs
: eterm-key-ior ( -- char|ior )
  begin
    default-key-ior
    dup 'C-z' = while
    drop
    parse-eterm-command
    2dup execute-eterm-command
    drop free throw
  repeat
;

' eterm-key-ior ' default-key? input: eterm-input-vector

: say-hello ( -- )
  s" Welcome to Gforth " cmd, version-string cmd, s" !" cmd,
;

\ Normally Gforth configures the terminal so that pressing C-z
\ generates a SIGTSTP signal.  (Gforth seems to ignore SIGTSTP.)  We
\ disable VSUSP so that C-z can be read by KEY.
: prepare-terminal ( -- )
  s" stty susp undef" system
  eterm-input-vector
;

: setup ( -- )
  ['] say-hello send-emacs-cmd
  prepare-terminal
  expand-where  \ WHERE must print full filenames, even if it's ugly
  2 to after-locate \ reduce clutter in the *forth-term* buffer a bit
;

setup

only forth
