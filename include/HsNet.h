/* -----------------------------------------------------------------------------
 * $Id: HsNet.h,v 1.5 2002/05/01 23:20:21 ken Exp $
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

#ifdef osf3_TARGET_OS
# define _SOCKADDR_LEN
#endif
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

extern StgInt
sendAncillary(int sock,
	      int level,
	      int type,
	      int flags,
	      void* data,
	      int len);

extern StgInt
recvAncillary(int  sock,
	      int* pLevel,
	      int* pType,
	      int  flags,
	      void** pData,
	      int* pLen);


#endif /* HAVE_WINSOCK_H && !__CYGWIN */

#endif
