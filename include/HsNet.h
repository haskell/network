/* -----------------------------------------------------------------------------
 *
 * Definitions for package `net' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSNET_H
#define HSNET_H

#include "HsNetworkConfig.h"

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# elif defined(__GNUC__)
#  define INLINE extern inline
# else
#  define INLINE inline
# endif
#endif

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)
#include <winsock.h>

extern void  shutdownWinSock();
extern int   initWinSock ();
extern const char* getWSErrorDescr(int err);

# if !defined(__HUGS__)
extern void* newAcceptParams(int sock,
			     int sz,
			     void* sockaddr);
extern int   acceptNewSock(void* d);
extern int   acceptDoProc(void* param);
# endif

#else

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_SYS_UIO_H
# include <sys/uio.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_SYS_UN_H
# include <sys/un.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#ifdef HAVE_BSD_SENDFILE
#include <sys/uio.h>
#endif
#ifdef HAVE_LINUX_SENDFILE
#if !defined(__USE_FILE_OFFSET64)
#include <sys/sendfile.h>
#endif
#endif

extern int
sendFd(int sock, int outfd);

extern int
recvFd(int sock);

/* The next two are scheduled for deletion */
extern int
sendAncillary(int sock,
	      int level,
	      int type,
	      int flags,
	      void* data,
	      int len);

extern int
recvAncillary(int  sock,
	      int* pLevel,
	      int* pType,
	      int  flags,
	      void** pData,
	      int* pLen);

#endif /* HAVE_WINSOCK_H && !__CYGWIN */

INLINE char *
my_inet_ntoa(
#if defined(HAVE_WINSOCK_H)
             u_long addr
#elif defined(HAVE_IN_ADDR_T)
             in_addr_t addr
#elif defined(HAVE_INTTYPES_H)
             u_int32_t addr
#else
             unsigned long addr
#endif
	    )
{ 
    struct in_addr a;
    a.s_addr = addr;
    return inet_ntoa(a);
}

#endif
