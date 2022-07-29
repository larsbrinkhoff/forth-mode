also externals \ for ERRNO

localextern: %write int write ( int, *, size_t )
localextern: %read int read ( int, *, size_t )
localextern: %pipe int pipe ( * )

localextern: %opendir * opendir ( * )
localextern: %closedir int closedir ( * )
localextern: %readdir * readdir ( * )

localextern: %stat int stat ( *, * )

: %errno ( -- n ) errno @ ;
: %errno! ( n -- ) errno ! ;
: %/fd ( -- u ) 4 ;

\ Load the U bytes at ADDR and return them as integer in little endian
\ format.
: %le-u@ ( addr u -- u2 )
  0 swap 0 ?do			( addr u2'  )
    over i + c@			( addr u2' byte )
    i 8 * lshift or		( addr u2' )
  loop
  nip
;

\ If the bit at BITPOS in U is set, then also set the higher bits.
: %sign-extend ( u bitpos -- n )
  {: u bitpos :}
  u 1 bitpos lshift and if
    u -1 bitpos lshift or
  else
    u
  then
;

: %le-n@ ( addr u -- n ) tuck %le-u@ swap 8 * 1- %sign-extend ;

: %fd@ ( fd-addr -- fd )
  %/fd 1 cells = if
    @
  else
    %/fd %le-n@
  then
;

\ Hardcoded offsets for 32/64-bit linux
1 cells 4 = [if]
$b constant %d_name_offset
$10 constant %st_mode_offset
$58 constant %/stat
[else]
$13 constant %d_name_offset
$18 constant %st_mode_offset
$90 constant %/stat
[then]

$f000 constant %S_IFMT
$4000 constant %S_IFDIR
$8000 constant %S_IFREG

: %dirent-name ( dirent -- zstring ) %d_name_offset + ;
: %stat-mode ( stat-addr -- mode ) %st_mode_offset + @ ;
: %s-isdir ( mode -- ) %S_IFMT and %S_IFDIR = ;
: %s-isreg ( mode -- ) %S_IFMT and %S_IFREG = ;

4  constant %eintr
13 constant %eacces

previous


