\ indexer.fth --- Parse source files and record stuff in the data base.

require parser.fth
require db.fth
require file-reader.fth
require file.fth
require textdoc.fth

0
1 cells +field %indexer-db
/parser +field %indexer-parser
2 cells +field %indexer-uri
constant /indexer

: %indexer-record-definition ( indexer -- )
  dup %indexer-parser {: mk p :}
  p %parser-scan
  mk %indexer-uri 2@
  p %parser-line @
  p %parser-column @
  mk %indexer-db @
  db-insert-definition
;

: %%indexer-skip ( terminator char -- terminator|-1 flag )
  over -1 = if 2drop -1 false exit then
  over = if drop -1 true exit then
  true
;

: %indexer-skip ( terminator indexer -- )
  ['] %%indexer-skip swap %indexer-parser parser-skip drop
;

wordlist constant indexer-wordlist
get-current indexer-wordlist set-current

: : ( indexer -- ) %indexer-record-definition ;
: +field ( indexer -- ) %indexer-record-definition ;
: constant ( indexer -- ) %indexer-record-definition ;
: value ( indexer -- ) %indexer-record-definition ;
: create ( indexer -- ) %indexer-record-definition ;
: variable ( indexer -- ) %indexer-record-definition ;
: begin-structure ( indexer -- ) %indexer-record-definition ;
: synonym ( indexer -- ) %indexer-record-definition ;

: 2constant ( indexer -- ) %indexer-record-definition ;
: 2value ( indexer -- ) %indexer-record-definition ;
: 2variable ( indexer -- ) %indexer-record-definition ;

: field: ( indexer -- ) %indexer-record-definition ;
: cfield: ( indexer -- ) %indexer-record-definition ;
: ffield: ( indexer -- ) %indexer-record-definition ;
: dffield: ( indexer -- ) %indexer-record-definition ;
: sffield: ( indexer -- ) %indexer-record-definition ;

: s" ( indexer -- ) '"' swap %indexer-skip ;
: ." ( indexer -- ) '"' swap %indexer-skip ;
: c" ( indexer -- ) '"' swap %indexer-skip ;
: .( ( indexer -- ) ')' swap %indexer-skip ;
: ( ( indexer -- ) ')' swap %indexer-skip ;
: \ ( indexer -- ) 'LF' swap %indexer-skip ;

set-current

: init-indexer ( db a-addr -- indexer )
  >r
  r@ %indexer-db !
  indexer-wordlist r@ r@ %indexer-parser init-parser drop
  r>
;

: %indexer-delete-stale-definitions ( uri$ indexer -- )
  %indexer-db @ db-delete-defintions-with-uri
;

: %indexer-process-uri ( reader uri$ indexer -- )
  >r
  r@ %indexer-uri 2!
  r> %indexer-parser parser-parse
;

: indexer-process-file ( filename$ indexer -- )
  rot rot 2dup make-file-uri 2swap make-file-reader
  {: indexer u ulen r :}
  r u ulen indexer %indexer-process-uri
  r drop-file-reader
  u ulen drop-file-uri
;

: %indexer-process-directory ( indexer file$ -- indexer )
  {: indexer file file-len :}
  file file-len s" .fth" string-suffix? if
    file file-len indexer indexer-process-file
  then
  indexer
;

: indexer-process-directory ( c-addr u indexer -- )
  ['] %indexer-process-directory 2swap traverse-directory
  drop
;

: indexer-process-textdoc ( textdoc indexer -- )
  swap dup make-textdoc-reader {: indexer doc r :}
  r doc textdoc-uri
  2dup indexer %indexer-delete-stale-definitions
  indexer %indexer-process-uri
  r drop-textdoc-reader
;
