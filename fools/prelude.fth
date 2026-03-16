\ prelude.fth --- This file is loaded first
\
\ Here we bring all supported implementations to a (minimal) common
\ level.  In particular, we change Gforth's load path so that REQUIRE
\ interprets file names relative to the process's working directory,
\ like the other implementations do.

: gforth? ( -- flag ) s" gforth" environment? if 2drop true else false then ;
: vfxforth? ( -- flag ) [ [defined] vfxforth ] literal ;
: swiftforth? ( -- flag ) [ [defined] enter-swift ] literal ;

: unknown-forth? ( -- flag )
  gforth? if false exit then
  vfxforth? if false exit then
  swiftforth? if false exit then
  true
;

gforth? [if] fpath path= ~+ [then]
gforth? [if] require prelude/gforth.fth [then]

wordlist constant fools-wordlist
forth-wordlist fools-wordlist 2 set-order definitions
