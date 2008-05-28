AC_INIT([Haskell network package], [1.0], [libraries@haskell.org], [network])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsNet.h])

AC_CONFIG_HEADERS([include/HsNetworkConfig.h])

AC_CANONICAL_HOST

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_C_CONST

dnl ** check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([fcntl.h limits.h stdlib.h sys/types.h unistd.h winsock.h])
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h netinet/tcp.h sys/socket.h sys/uio.h sys/un.h])

AC_CHECK_FUNCS([readlink symlink])

dnl ** check what fields struct msghdr contains
AC_CHECK_MEMBERS([struct msghdr.msg_control, struct msghdr.msg_accrights], [], [], [#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#if HAVE_SYS_UIO_H
# include <sys/uio.h>
#endif])

dnl --------------------------------------------------
dnl * test for in_addr_t
dnl --------------------------------------------------
AC_MSG_CHECKING(for in_addr_t in netinet/in.h)
AC_EGREP_HEADER(in_addr_t, netinet/in.h,
 [ AC_DEFINE([HAVE_IN_ADDR_T], [1], [Define to 1 if in_addr_t is available.]) AC_MSG_RESULT(yes) ],
 AC_MSG_RESULT(no))

dnl --------------------------------------------------
dnl * test for SO_PEERCRED and struct ucred
dnl --------------------------------------------------
AC_MSG_CHECKING(for SO_PEERCRED and struct ucred in sys/socket.h)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#include <sys/socket.h>
#ifndef SO_PEERCRED
# error no SO_PEERCRED
#endif
struct ucred u;]])],ac_cv_ucred=yes,ac_cv_ucred=no)
if test "x$ac_cv_ucred" = xno; then
    old_CFLAGS="$CFLAGS"
    CFLAGS="-D_GNU_SOURCE $CFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#include <sys/socket.h>
#ifndef SO_PEERCRED
# error no SO_PEERCRED
#endif
struct ucred u;]])],ac_cv_ucred=yes,ac_cv_ucred=no)
    if test "x$ac_cv_ucred" = xyes; then
        EXTRA_CPPFLAGS=-D_GNU_SOURCE
    fi
else
    old_CFLAGS="$CFLAGS"
fi
if test "x$ac_cv_ucred" = xno; then
    CFLAGS="$old_CFLAGS"
    AC_MSG_RESULT(no)
else
    AC_DEFINE([HAVE_STRUCT_UCRED], [1], [Define to 1 if you have both SO_PEERCRED and struct ucred.])
    AC_MSG_RESULT(yes)
fi

dnl --------------------------------------------------
dnl * test for getaddrinfo as proxy for IPv6 support
dnl --------------------------------------------------
AC_CHECK_FUNCS(getaddrinfo)

dnl -------------------------------------------------------
dnl * test for AI_* flags that not all implementations have
dnl -------------------------------------------------------
AC_CHECK_DECLS([AI_ADDRCONFIG, AI_ALL, AI_NUMERICSERV, AI_V4MAPPED], , ,
 [#include <netdb.h>])

dnl --------------------------------------------------
dnl * test for Linux sendfile(2)
dnl --------------------------------------------------
AC_MSG_CHECKING(for sendfile in sys/sendfile.h)
AC_EGREP_HEADER(sendfile, sys/sendfile.h,
 [ AC_DEFINE([HAVE_LINUX_SENDFILE], [1], [Define to 1 if you have a Linux sendfile(2) implementation.]) AC_MSG_RESULT(yes) ],
 AC_MSG_RESULT(no))

dnl --------------------------------------------------
dnl * test for BSD sendfile(2)
dnl --------------------------------------------------
AC_MSG_CHECKING(for sendfile in sys/socket.h)
AC_EGREP_HEADER(sendfile, sys/socket.h,
 [ AC_DEFINE([HAVE_BSD_SENDFILE], [1], [Define to 1 if you have a BSDish sendfile(2) implementation.]) AC_MSG_RESULT(yes) ],
 AC_MSG_RESULT(no))

AC_CHECK_FUNCS(gethostent)

case "$host" in
*-mingw32)
	EXTRA_SRCS="cbits/initWinSock.c, cbits/winSockErr.c, cbits/asyncAccept.c"
	EXTRA_LIBS=wsock32
	CALLCONV=stdcall ;;
*-solaris2*)
	EXTRA_SRCS="cbits/ancilData.c"
	EXTRA_LIBS="nsl, socket"
	CALLCONV=ccall ;;
*)
	EXTRA_SRCS="cbits/ancilData.c"
	EXTRA_LIBS=
	CALLCONV=ccall ;;
esac
AC_SUBST([CALLCONV])
AC_SUBST([EXTRA_CPPFLAGS])
AC_SUBST([EXTRA_LIBS])
AC_SUBST([EXTRA_SRCS])

AC_CONFIG_FILES([network.buildinfo])

AC_OUTPUT
