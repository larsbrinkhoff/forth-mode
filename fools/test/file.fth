require prelude.fth
require file.fth
require tap.fth
require string.fth

: test-stat ( -- )
  s" test/file.fth" regular-file? ok
  s" test" regular-file? 0= ok
  s" test/file.fth" directory? 0= ok
  s" test" directory? ok
  s\" test/file.fth\z" drop %regular-file? ok
  s\" test\z" drop %regular-file? 0= ok
;

: test-directory-name ( -- )
  s\" a/\z" drop %directory-name? ok
  s\" a\z" drop %directory-name? 0= ok
  s\" /\z" drop %directory-name? ok
  s\" \z" drop %directory-name? 0= ok
;

: test-join-file-names ( -- )
  s\" a\z" drop s\" b\z" drop %join-file-names
  dup strlen 1+ s\" a/b\z" compare 0= ok

  s\" a/\z" drop s\" b\z" drop %join-file-names
  dup strlen 1+ s\" a/b\z" compare 0= ok

  s\" .\z" drop s\" b\z" drop %join-file-names
  dup strlen 1+ s\" ./b\z" compare 0= ok

  s\" a\z" drop s\" b/c/d\z" drop %join-file-names
  dup strlen 1+ s\" a/b/c/d\z" compare 0= ok
;

: f1 ( found? c-addr u -- found? )
  2dup directory? if ." directory: " type cr false ok then
  s" test/file.fth" string-suffix? or
;

: test-traverse-directory ( -- )
  false ['] f1 s" ." traverse-directory ok
;

: main ( -- )
  test-stat
  test-directory-name
  test-join-file-names
  test-traverse-directory
;

' main 16 run-tests
