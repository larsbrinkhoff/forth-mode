\ textdoc.fth --- In memory representation of a text file.

require file-reader.fth
require parser.fth
require uri.fth
require gapbuffer.fth
require misc.fth
require gapbuffer-reader.fth
require vector.fth

0
2 cells    +field %textdoc-uri	     \ uri (string)
/gapbuffer +field %textdoc-gapbuffer \ gapbuffer
constant /textdoc

: init-textdoc ( uri$ text$ a-addr -- textdoc )
  >r
  dup r@ %textdoc-gapbuffer init-gapbuffer 0 swap gapbuffer-insert
  copy-slice r@ %textdoc-uri 2!
  r>
;

: deinit-textdoc ( textdoc -- )
  {: doc :}
  doc %textdoc-uri 2@ drop free throw
  doc %textdoc-gapbuffer deinit-gapbuffer
;

: textdoc-uri ( textdoc -- uri$ ) %textdoc-uri 2@ ;

: textdoc-change ( start-line start-col
		   end-line end-col
		   text$ textdoc -- invalid-position? )
  %textdoc-gapbuffer {: sl sc el ec t tlen b :}
  sl sc b gapbuffer-position>index if drop true exit then
  el ec b gapbuffer-position>index if 2drop true exit then
  t tlen b gapbuffer-change
  false
;

: textdoc-valid-position? ( line col textdoc -- flag )
  %textdoc-gapbuffer gapbuffer-position>index nip 0=
;

: make-textdoc-reader ( textdoc -- reader )
  %textdoc-gapbuffer make-gapbuffer-reader
;

: drop-textdoc-reader ( reader -- ) drop-gapbuffer-reader ;

: .textdoc-text ( textdoc -- )
  %textdoc-gapbuffer %gapbuffer-copy 2dup type
  drop free throw
;
