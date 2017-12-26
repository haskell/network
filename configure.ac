AC_INIT([Haskell network package], [3.0.0.0], [libraries@haskell.org], [network])

ac_includes_default="#define _GNU_SOURCE 1
$ac_includes_default
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_NETDB_H
# include <netdb.h>
#endif
#ifdef HAVE_SYS_UIO_H
# include <sys/uio.h>
#endif
#ifdef HAVE_WINSOCK2_H
# include <winsock2.h>
#endif
#ifdef HAVE_WS2TCPIP_H
# include <ws2tcpip.h>
// fix for MingW not defining IPV6_V6ONLY
# define IPV6_V6ONLY 27
#endif"

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
AC_CHECK_HEADERS([fcntl.h limits.h stdlib.h sys/types.h unistd.h winsock2.h ws2tcpip.h])
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h netinet/tcp.h sys/socket.h sys/uio.h sys/un.h linux/tcp.h])
AC_CHECK_HEADERS([net/if.h])

AC_CHECK_TYPES([struct ucred])

AC_CHECK_FUNCS([readlink symlink if_nametoindex])
AC_CHECK_FUNCS([gai_strerror gethostent accept4])
AC_CHECK_FUNCS([getpeereid])

AC_CHECK_DECLS([AI_ADDRCONFIG, AI_ALL, AI_NUMERICSERV, AI_V4MAPPED])
AC_CHECK_DECLS([IPV6_V6ONLY])
AC_CHECK_DECLS([IPPROTO_IP, IPPROTO_TCP, IPPROTO_IPV6])
AC_CHECK_DECLS([getaddrinfo])
AC_CHECK_DECLS([SO_PEERCRED])

AC_CHECK_MEMBERS([struct msghdr.msg_control, struct msghdr.msg_accrights], [], [], [])
AC_CHECK_MEMBERS([struct sockaddr.sa_len], [], [], [])

AC_CHECK_LIB(ws2_32, _head_libws2_32_a)

case "$host" in
*-mingw* | *-msys*)
	EXTRA_SRCS="cbits/initWinSock.c, cbits/winSockErr.c, cbits/asyncAccept.c"
	EXTRA_LIBS=ws2_32 ;;
*-solaris2*)
	EXTRA_SRCS="cbits/ancilData.c"
	EXTRA_LIBS="nsl, socket" ;;
*)
	EXTRA_SRCS="cbits/ancilData.c"
	EXTRA_LIBS= ;;
esac
AC_SUBST([CALLCONV])
AC_SUBST([EXTRA_CPPFLAGS])
AC_SUBST([EXTRA_LIBS])
AC_SUBST([EXTRA_SRCS])

AC_CONFIG_FILES([network.buildinfo])

AC_OUTPUT
