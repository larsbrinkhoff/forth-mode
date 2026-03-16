
\ SwiftForth defines various things in src/kernel/linux/import.f.
\ That's where WRITE, READ, STAT and ERRNO come from.

function: pipe ( a -- n )
function: opendir ( a -- a )
function: closedir ( a -- n )
function: readdir ( a -- a )

synonym %write write
synonym %read read
synonym %pipe pipe

synonym %opendir opendir
synonym %closedir closedir
synonym %readdir readdir

synonym %stat stat

: %errno ( -- n ) errno @ ;
: %errno! ( n -- ) errno ! ;
: %/fd 4 ;
: %fd@ @ ;

\ 32-bit linux
$b constant %d_name_offset
$10 constant %st_mode_offset
$58 constant %/stat

\ #define	__S_IFMT	0170000	/* These bits determine file type.  */
\ #define	__S_IFDIR	0040000	/* Directory.  */
\ #define	__S_IFREG	0100000	/* Regular file.  */

$f000 constant %S_IFMT
$4000 constant %S_IFDIR
$8000 constant %S_IFREG

: %dirent-name ( dirent -- zstring ) %d_name_offset + ;
: %stat-mode ( stat-addr -- mode ) %st_mode_offset + @ ;
: %s-isdir ( mode -- ) %S_IFMT and %S_IFDIR = ;
: %s-isreg ( mode -- ) %S_IFMT and %S_IFREG = ;

4  constant %eintr
13 constant %eacces
