
c-library posix

\c #include <stdlib.h>
\c #include <errno.h>
\c #include <unistd.h>
\c #include <dirent.h>
\c #include <sys/stat.h>

c-function %read read n a n -- n
c-function %write write n a n -- n
c-function %pipe pipe a -- n

\c static int get_errno (void) { return errno; }
c-function %errno get_errno -- n

\c static void set_errno (int i) { errno = i; }
c-function %errno! set_errno n -- void

\c static size_t fd_size (void) { return sizeof (int); }
c-function %/fd fd_size -- n

\c static int load_fd (int* addr) { return *addr; }
c-function %fd@ load_fd a -- n

c-function %opendir opendir a -- a
c-function %closedir closedir a -- n
c-function %readdir readdir a -- a

\c static char* dirent_name (struct dirent* de) { return de->d_name; }
c-function %dirent-name dirent_name a -- a

c-function %stat stat a a -- n

\c static size_t stat_size (void) { return sizeof (struct stat); }
c-function %/stat stat_size -- n

\c static size_t stat_mode (struct stat* s) { return s->st_mode; }
c-function %stat-mode stat_mode a -- n

\c static int s_isdir (mode_t m) { return S_ISDIR (m); }
c-function %s-isdir s_isdir n -- n

\c static int s_isreg (mode_t m) { return S_ISREG (m); }
c-function %s-isreg s_isreg n -- n

\c static size_t eintr (void) { return EINTR; }
c-function %eintr eintr  -- n

\c static size_t eacces (void) { return EACCES; }
c-function %eacces eacces -- n

c-function %realpath realpath a a -- a
c-function %free free a -- void

end-c-library
