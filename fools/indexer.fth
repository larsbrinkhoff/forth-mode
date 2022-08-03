\ indexer.fth --- Parse source files and record stuff in the data base.

require parser.fth
require db.fth
require file-reader.fth
require file.fth
require textdoc.fth
require region.fth

begin-structure /indexer
  field:         %indexer-db
  /parser +field %indexer-parser
  2 cells +field %indexer-uri
  /region +field %indexer-scratch
end-structure

: %indexer-record-definition ( indexer -- )
  dup %indexer-parser {: mk p :}
  p %parser-scan
  mk %indexer-uri 2@
  p %parser-line @
  p %parser-column @
  s" "
  mk %indexer-db @
  db-insert-definition
;

: %indexer-begin-string ( indexer -- )
  assert( dup %indexer-scratch region-object-size 0= )
  drop
;

: %indexer-grow-string ( string$ indexer -- )
  %indexer-scratch region-add-slice
;

: %indexer-finish-string ( indexer -- string$ )
  %indexer-scratch
  dup region-object-size
  swap region-finish
  swap
;

: %indexer-free-string ( string$ indexer -- )
  nip %indexer-scratch region-free
;

: %indexer-string-to-scratch ( string$ indexer -- string2$ )
  {: i :}
  i %indexer-begin-string
  i %indexer-grow-string
  i %indexer-finish-string
;

: %indexer-parse-comment ( indexer -- comment$ next-token$ )
  dup %indexer-parser {: indexer p :}
  p %parser-scan      ( token$ )
  2dup s" (" compare 0<> if s" " 2swap exit then
  indexer %indexer-begin-string
  indexer %indexer-grow-string
  begin
    p %parser-scan      ( token$' )
    2dup s" )" compare 0= if
      s"  " indexer %indexer-grow-string
      indexer %indexer-grow-string
      indexer %indexer-finish-string ( comment$ )
      p %parser-scan
      exit
    else
      s"  " indexer %indexer-grow-string
      indexer %indexer-grow-string
    then
  again
;

: %indexer-colon-definition ( indexer -- )
  dup %indexer-parser {: indexer p :}
  p %parser-scan      ( $name )
  indexer %indexer-string-to-scratch ( $name' )
  indexer %indexer-uri 2@
  p %parser-line @
  p %parser-column @
  indexer %indexer-parse-comment 2>r
  indexer %indexer-db @ db-insert-definition
  indexer %indexer-scratch region-free-all
  2r> p %parser-dispatch
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

: : ( indexer -- ) %indexer-colon-definition ;
: +field ( indexer -- ) %indexer-record-definition ;
: constant ( indexer -- ) %indexer-record-definition ;
: value ( indexer -- ) %indexer-record-definition ;
: create ( indexer -- ) %indexer-record-definition ;
: variable ( indexer -- ) %indexer-record-definition ;
: begin-structure ( indexer -- ) %indexer-record-definition ;
: synonym ( indexer -- ) %indexer-record-definition ;
: buffer: ( indexer -- ) %indexer-record-definition ;

: defer ( indexer -- ) %indexer-record-definition ;
: is ( indexer -- ) %indexer-record-definition ;

: 2constant ( indexer -- ) %indexer-record-definition ;
: 2value ( indexer -- ) %indexer-record-definition ;
: 2variable ( indexer -- ) %indexer-record-definition ;

: field: ( indexer -- ) %indexer-record-definition ;
: cfield: ( indexer -- ) %indexer-record-definition ;
: ffield: ( indexer -- ) %indexer-record-definition ;
: dffield: ( indexer -- ) %indexer-record-definition ;
: sffield: ( indexer -- ) %indexer-record-definition ;

: code ( indexer -- ) %indexer-record-definition ;

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
  r@ %indexer-scratch init-region drop
  r>
;

: %indexer-delete-stale-definitions ( uri$ indexer -- )
  %indexer-db @ db-delete-definitions-with-uri
;

: %indexer-process-uri ( reader uri$ indexer -- )
  >r
  r@ %indexer-uri 2!
  r> %indexer-parser parser-parse
;

: %indexer-process-file ( filename$ delete? indexer -- )
  2swap 2dup make-file-uri 2swap make-file-reader
  {: delete? indexer uri urilen r :}
  delete? if
    uri urilen indexer %indexer-delete-stale-definitions
  then
  r uri urilen indexer %indexer-process-uri
  r drop-file-reader
  uri urilen drop-file-uri
;

: indexer-process-file ( filename$ indexer -- )
  false swap %indexer-process-file
;

: indexer-reprocess-file ( filename$ indexer -- )
  true swap %indexer-process-file
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
