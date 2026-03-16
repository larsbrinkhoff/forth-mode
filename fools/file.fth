\ file.fth -- File system operations.
\
\ Layer on top of posix functions to hide some of the low level
\ details.

require posix.fth
require zstring.fth
require misc.fth
require string.fth

: %%%with-stat ( xt[stat-addr ior -- ] zstring stat-addr -- )
  tuck posix-stat -1 = if posix-errno else 0 then
  rot execute
;

: %%with-stat ( xt[stat-addr ior -- ] zstring -- )
  ['] %%%with-stat posix-/stat with-mem
;

: %with-stat ( xt[stat-addr ior -- ] c-addr u -- )
  ['] %%with-stat rot rot with-zstring
;

: %%with-dir ( xt[dir-addr ior -- ] zstring -- )
  posix-opendir {: xt dir :}
  dir dir 0= if posix-errno else 0 then xt catch
  dir 0<> if
    dir posix-closedir -1 = if posix-errno throw then
  then
  throw
;

: %with-dir ( xt[dir-addr ior -- ] c-addr u -- )
  ['] %%with-dir rot rot with-zstring
;

: %%regular-file? ( stat-addr ior -- flag )
  0= if posix-stat-mode posix-regular-file? else false then
;

: %regular-file? ( zstring -- flag ) ['] %%regular-file? swap %%with-stat ;
: regular-file? ( c-addr u -- flag ) ['] %%regular-file? rot rot %with-stat ;

: %%directory? ( stat-addr ior -- flag )
  0= if posix-stat-mode posix-directory? else false then
;

: %directory? ( zstring -- flag ) ['] %%directory? swap %%with-stat ;
: directory? ( c-addr u -- flag ) ['] %%directory? rot rot %with-stat ;

: %%file-mode ( stat-addr ior -- mode ior ) >r posix-stat-mode r> ;
: %file-mode ( zstring -- mode ior ) ['] %%file-mode swap %%with-stat ;

: %file-name-absolute? ( filename -- flag ) zcount s" /" 2swap string-prefix? ;
: %directory-name? ( filename -- flag )     zcount s" /" string-suffix? ;

: %join-file-names ( filename1 filename2 -- filename3 )
  >r
  dup %directory-name? if zcount 1- else zcount then
  r> zcount
  rot 2dup + 1+ dup 1+ allocate throw
  {: f1 f2 f2-len f1-len f3-len f3 :}
  f1 f3 f1-len move
  '/' f3 f1-len + c!
  f2 f3 f1-len 1+ + f2-len move
  0 f3 f3-len + c!
  f3
;

\ : %expand-file-name ( filename directory -- absolute )
\   swap %join-file-names		( joined )
\   dup 0 posix-realpath swap free throw
\   dup 0= if posix-errno throw then
\ ;

defer %traverse-directory-deferred

: %%%traverse-directory ( xt[c-addr u -- ] filename -- )
  dup %file-mode {: xt f m ior :}
  ior 0<> if exit then
  m posix-regular-file? if f zcount xt execute exit then
  m posix-directory?    if xt f %traverse-directory-deferred exit then
;

: %%traverse-directory ( xt[c-addr u -- ] dirname dir-addr ior -- )
  {: xt dirname dir ior :}
  ior posix-eacces = if exit then
  ior throw
  begin
    0 posix-errno!
    dir posix-readdir dup 0= if drop posix-errno throw exit then
    posix-dirent-name >r
    r@ zcount s" ." compare 0=
    r@ zcount s" .." compare 0= or if
      r> drop
    else
      dirname r> %join-file-names >r
      xt r@ %%%traverse-directory
      r> free throw
    then
  again
;

: %traverse-directory ( xt[c-addr u -- ] zstring -- )
  ['] %%traverse-directory over %%with-dir
;

' %traverse-directory is  %traverse-directory-deferred

\ Call XT with the name for each regular file in directory DIR or one
\ of its sub-directories.
: traverse-directory ( xt[c-addr u -- ] dir$ -- )
  ['] %traverse-directory rot rot with-zstring
;
