/* -----------------------------------------------------------------------------
 * $Id: HsNet.h,v 1.9 2002/10/19 10:42:24 stolz Exp $
 *
 * Definitions for package `net' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSNET_H
#define HSNET_H

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)
#include <winsock.h>

extern void   shutdownWinSock();
extern StgInt initWinSock ();
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

#ifndef HAVE_IN_ADDR_T
typedef	u_int32_t	in_addr_t;
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


#ifndef INLINE
#define INLINE extern inline
#endif

INLINE char *
my_inet_ntoa(in_addr_t addr)
{ 
    struct in_addr a;
    a.s_addr = addr;
    return inet_ntoa(a);
}

#endif /* HAVE_WINSOCK_H && !__CYGWIN */

#endif
