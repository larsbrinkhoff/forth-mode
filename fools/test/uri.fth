require prelude.fth
require uri.fth
require tap.fth

: test-uri>scheme ( -- )
  s" file:test/lsp.fth" uri>scheme s" file" compare 0= ok
;

: test-uri>host ( -- )
  s" file:test/lsp.fth" uri>host s" " compare 0= ok
  s" file:///test/lsp.fth" uri>host s" " compare 0= ok
  s" file://foo/test/lsp.fth" uri>host s" foo" compare 0= ok
;

: test-uri>filename ( -- )
  s" file:test/lsp.fth" uri>filename s" test/lsp.fth" compare 0= ok
  s" file:///foo/bar" uri>filename s" /foo/bar" compare 0= ok
;

: test-make-file-uri ( -- )
  s" foo/bar.fth" make-file-uri
  2dup s" file:foo/bar.fth" compare 0= ok
  drop-file-uri
;

: test-uri= ( -- )
  s" file:/foo" s" file:///foo" uri= ok
  s" file://foo" s" file:///foo" uri= 0= ok
;

: main ( -- )
  test-uri>scheme
  test-uri>host
  test-uri>filename
  test-make-file-uri
  test-uri=
;

' main 10 run-tests
