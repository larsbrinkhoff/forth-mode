
: json-ref ( c-addr u json -- json2 )
  {: a u obj :}
  a u obj json-object-get	( json2 found? )
  0= if
    drop
    ." member missing: " a u type cr obj .json cr
    true abort" member-missing"
  then
;
