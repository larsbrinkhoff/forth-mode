\ db.fth --- In memory database for various things

require hashtable.fth
require hash.fth
require vector.fth
require uri.fth

begin-structure %/db-string
  field:   %db-string-length
  0 +field %db-string-contents
end-structure

begin-structure /definition
  field: %definition-name	\ db-string
  field: %definition-uri	\ uri (interned db-string)
  field: %definition-line	\ u
  field: %definition-column	\ u
  \ 1 cells    +field %definition-signature \ stack comment 0|db-string
end-structure

begin-structure %/db
  /hashtable +field %db-strings	    \ hashtable<db-string,db-string>
  /vector    +field %db-definitions \ vector<definition>
end-structure

: %db-string-slice  ( db-string -- c-addr u )
  dup %db-string-contents swap %db-string-length @
;

: %db-make-string ( c-addr u -- db-string )
  dup 1 cells + allocate throw >r
  dup r@ %db-string-length !
  swap r@ %db-string-contents rot move
  r>
;

: %db-drop-string ( db-string -- ) free throw ;

: %db-with-string ( xt[db-string -- ] c-addr u -- )
  %db-make-string >r
  r@ swap execute
  r> %db-drop-string
;

: %db-string= ( db-string1 db-string2 -- flag )
  %db-string-slice rot %db-string-slice compare 0=
;

: %db-string-hash ( db-string -- u ) %db-string-slice jhash ;

: %%db-lookup-string ( db db-string -- db-string2 found? )
  swap %db-strings hashtable-get
;

: %db-lookup-string ( c-addr u db -- db-string found? )
  ['] %%db-lookup-string 2swap %db-with-string
;

: %db-intern-string ( c-addr u db -- db-string )
  {: a u db :}
  a u db %db-lookup-string if exit else drop then
  a u %db-make-string
  dup dup db %db-strings hashtable-put
;

: %init-definition ( name uri line column a-addr -- def )
  >r
  r@ %definition-column !
  r@ %definition-line !
  r@ %definition-uri !
  r@ %definition-name !
  r>
;

: db-insert-definition ( name-slice uri-slice line column db -- )
  {: name namelen uri urilen line col db :}
  name namelen %db-make-string
  uri urilen db %db-intern-string
  line col /definition db %db-definitions vector-add-blank
  %init-definition drop
;

: %db-name-match? ( c-addr u definition -- c-addr u flag )
  %definition-name @ %db-string-slice 2over string-ci=
;

: db-select-definitions ( c-add u db -- vector<definition> )
  ['] %db-name-match? /definition rot %db-definitions vector-filter
  nip nip
;

: %db-uri-match? ( uri$ definition -- uri$ flag )
  %definition-uri @ %db-string-slice 2over uri=
;

: db-delete-definitions-with-uri ( uri$ db -- )
  ['] %db-uri-match? /definition rot %db-definitions vector-delete 2drop
;

: %init-db ( a-addr -- db )
  >r
  ['] %db-string-hash ['] %db-string= 0 default-allocator
  r@ %db-strings init-hashtable drop
  r@ %db-definitions init-vector drop
  r>
;

: make-db ( -- db ) %/db allocate throw %init-db ;

: .definition ( definition -- )
  {: d :}
  d %definition-name @ %db-string-slice type ."  "
  d %definition-uri @ %db-string-slice type ." :"
  d %definition-line @ 0 u.r ." :"
  d %definition-column @ 0 u.r
;

: db-list-definitions ( db -- )
  %db-definitions vector-bounds ?do
    i .definition cr
    /definition
  +loop
;
