AC_INIT([Haskell network package], [1.0], [libraries@haskell.org], [network])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/HsNet.h])

AC_CONFIG_HEADERS([include/HsNetworkConfig.h])

AC_CANONICAL_HOST

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
*-solaris2)
	EXTRA_SRCS="cbits/ancilData.c"
	EXTRA_LIBS="nsl, socket"
	CALLCONV=ccall ;;
*)
	EXTRA_SRCS="cbits/ancilData.c"
	EXTRA_LIBS=
	CALLCONV=ccall ;;
esac
AC_SUBST([CALLCONV])
AC_SUBST([EXTRA_LIBS])
AC_SUBST([EXTRA_SRCS])

AC_CONFIG_FILES([network.buildinfo])

AC_OUTPUT
