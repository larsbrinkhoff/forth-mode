\ posix.fth --- POSIX functions
\
\ This file provides access to some of the POSIX functions like pipes
\ and directories.  Unfortuntely, this requires FFI calls and that is
\ different in each implementation.
\
\ (Would it make sense to implement a FFI portability layer?)

gforth? [if] require posix/gforth.fth [then]
swiftforth? [if] require posix/swiftforth.fth [then]
vfxforth? [if] require posix/vfxforth.fth [then]

: posix-write ( fd c-addr u -- n ) %write ;
: posix-read ( fd c-addr u -- n ) %read ;
: posix-pipe ( fd-pair-addr -- n ) %pipe ;
: posix-fd-pair-get ( u fd-pair-addr -- fd ) swap %/fd * + %fd@ ;

: posix-errno ( -- u ) %errno ;
: posix-errno! ( u -- ) %errno! ;

: posix-eintr ( -- u )  %eintr ;
: posix-eacces ( -- u ) %eacces ;

: posix-opendir ( zstring -- dir-addr ) %opendir ;
: posix-closedir ( dir-addr -- n ) %closedir ;
: posix-readdir ( dir-addr -- dirent-addr ) %readdir ;
: posix-dirent-name ( dirent-addr -- zstring ) %dirent-name ;

: posix-stat ( zstring stat-addr -- n ) %stat ;
: posix-/stat ( -- u ) %/stat ;
: posix-stat-mode ( stat-addr -- mode ) %stat-mode ;
: posix-directory? ( mode -- flag ) %s-isdir ;
: posix-regular-file? ( mode -- flag ) %s-isreg ;

\ : posix-realpath ( zstring c-addr -- zstring2 ) %realpath ;
\ : posix-free ( a-addr -- ) %free ;
