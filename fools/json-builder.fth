\ json-builder.fth --- Helpers to construct JSON values.

require json.fth
require region.fth
require utf8.fth

begin-structure /jb
  field:         %jb-region  \ region& where values will be allocated
  /region +field %jb-stack   \ stack for building arrays and objects.
  /region +field %jb-marks   \ marks the beginning of current array/object
end-structure

: init-jb ( region a-addr -- jb )
  >r
  r@ %jb-region !
  r@ %jb-stack init-region drop
  r@ %jb-marks init-region drop
  r>
;

: deinit-jb ( jb -- )
  dup %jb-stack deinit-region
  %jb-marks deinit-region
;

: jb-begin-string ( jb -- )
  %jb-region @
  assert( dup region-object-size 0= )
  %/json-header swap region-blank
;

: jb-add-codepoint ( codepoint jb -- )
  %jb-region @ swap dup utf8-encoded-size ( region codepoint len )
  rot region-add utf8-encode
;

: jb-add-byte ( char jb -- ) %jb-region @ region-add-byte ;

: jb-finish-string ( jb -- json )
  %jb-region @ dup region-object-size swap region-finish {: u a :}
  assert( a cell-aligned? )
  u %/json-header - %jbt-string a %json-header!
  a %json-string-tag %json-tag
;

: %jb-push-mark ( x jb -- ) %jb-marks region-add-cell ;

: %jb-pop-mark ( jb -- x )
  assert( dup %jb-marks region-object-size 1 cells u< 0= )
  dup %jb-marks region-next-free 1 cells - @ ( jb x )
  1 cells negate rot %jb-marks region-blank
;

: %jb-mark ( jb -- )
  dup %jb-stack region-object-size ( jb stack-size )
  assert( dup cell-aligned? )
  swap %jb-marks region-add-cell
;

: jb-begin-array ( jb -- ) %jb-mark ;
: jb-push ( json jb -- ) %jb-stack region-add-cell ;

: jb-finish-array ( jb -- json )
  dup %jb-pop-mark {: jb mark :}
  jb %jb-stack region-base
  jb %jb-stack region-object-size mark /string	 ( a-addr u )
  tuck 1 cells / jb %jb-region @ json-make-array ( u json )
  swap negate jb %jb-stack region-blank
;

: jb-begin-object ( jb -- ) %jb-mark ;
: jb-push-member ( key value jb -- ) rot over jb-push jb-push ;

: jb-finish-object ( jb -- json )
  dup %jb-pop-mark {: jb mark :}
  jb %jb-stack region-base
  jb %jb-stack region-object-size mark /string	 ( a-addr u )
  assert( dup 2 cells mod 0= )
  tuck 2 cells / jb %jb-region @ json-make-object ( u json )
  swap negate jb %jb-stack region-blank
;

: jb-str ( c-addr u jb -- json ) %jb-region @ json-make-string ;
: jb-: ( key$ value jb -- ) {: v j :} j jb-str v j jb-push-member ;

synonym jb-{ jb-begin-object
synonym jb-} jb-finish-object
synonym jb-[ jb-begin-array
synonym jb-] jb-finish-array
synonym jb-, jb-push
