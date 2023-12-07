require json.fth
require writer.fth
require ascii.fth
require terminal-writer.fth

: %json-write ( c-addr u writer -- ior )
  {: u w :}
  u w writer-write ?dup if nip exit then
  u <> if $bad exit then
  0
;

: %json-write-int ( json writer -- ior )
  {: w :}
  json-integer-value
  dup 0< if abs s>d <# #s -1 sign #> else s>d <# #s #> then
  w %json-write
;

: %json-as-hex ( u -- c-addr u )
  base @ >r
  16 base !
  s>d <# #s #>
  r> base !
;

: %json-write-control ( char writer -- ior )
  {: w :}
  s" \u00"                         w %json-write    ?dup if exit then
  dup 4 rshift $f and %json-as-hex w %json-write    ?dup if exit then
  $f and              %json-as-hex w %json-write
;

: %json-write-string ( json writer -- ior )
  {: w :}
  '"' w writer-write-byte ?dup if exit then
  json-string-slice
  begin dup 0<> while
    over c@ case
      '\'         of s" \\"    w %json-write ?dup if exit then endof
      '"'         of s\" \\\"" w %json-write ?dup if exit then endof
      'LF'        of s" \n"    w %json-write ?dup if exit then endof
      'CR'        of s" \r"    w %json-write ?dup if exit then endof
      'HTAB'      of s" \t"    w %json-write ?dup if exit then endof
      'BACKSPACE' of s" \b"    w %json-write ?dup if exit then endof
      'FF'        of s" \f"    w %json-write ?dup if exit then endof
      dup $1f u> if   dup w writer-write-byte   ?dup if nip exit then
		 else dup w %json-write-control ?dup if nip exit then then
    endcase
    1 /string
  repeat
  2drop
  '"' w writer-write-byte
;

: json-write ( writer json -- ior )
  swap {: w :}
  dup %json-tag-of case
    %json-immediate-tag of
      %json-tag-bits rshift case
	%json-true-imm  of s" true"  w %json-write endof
	%json-false-imm of s" false" w %json-write endof
	%json-null-imm  of s" null"  w %json-write endof
	true abort" invalid json value (immediate)"
      endcase
    endof
    %json-fixnum-tag of w %json-write-int endof
    %json-string-tag of w %json-write-string endof
    %json-boxed-tag of
      dup %json-untag %json-header-type@ case
	%jbt-array of
	  s" [" w %json-write ?dup if exit then
	  %json-array-slice
	  begin dup 0<> while
	    over @ w swap recurse ?dup if nip nip exit then
	    1 cells /string
	    dup 0<> if s" , " w %json-write ?dup if nip nip exit then then
	  repeat
	  2drop
	  s" ]" w %json-write
	endof
	%jbt-object of
	  s" {" w %json-write                   ?dup if nip nip exit then
	  %json-object-slice
	  begin dup 0<> while
	    over @ w swap recurse               ?dup if nip nip exit then
	    s" :" w %json-write                 ?dup if nip nip exit then
	    over cell+ @ w swap recurse         ?dup if nip nip exit then
	    2 cells /string
	    dup 0<> if s" , " w %json-write     ?dup if nip nip exit then then
	  repeat
	  2drop
	  s" }" w %json-write
	endof
	true abort" invalid json value (boxed)"
      endcase
    endof
    true abort" invalid json value (unknown tag)"
  endcase
;

: .json ( json -- )
  terminal-writer swap json-write throw
  terminal-writer writer-flush throw
;
