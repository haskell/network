#include "HsNet.h"
#include "HsFFI.h"

#if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)

static int winsock_inited = 0;
static int winsock_uninited = 0;

/* Initialising WinSock... */
HsInt
initWinSock ()
{
  WORD wVersionRequested;
  WSADATA wsaData;  
  int err;
#ifdef __HUGS__
  int optval = SO_SYNCHRONOUS_NONALERT;
#endif

  if (!winsock_inited) {
    wVersionRequested = MAKEWORD( 1, 1 );

    err = WSAStartup ( wVersionRequested, &wsaData );
    
    if ( err != 0 ) {
       return err;
    }

    if ( LOBYTE( wsaData.wVersion ) != 1 ||
       HIBYTE( wsaData.wVersion ) != 1 ) {
      WSACleanup();
      return (-1);
    }
#ifdef __HUGS__
    /* By default, socket() creates sockets in overlapped mode
     * (so that async I/O is possible). The CRT can only handle
     * non-overlapped sockets, so turn off overlap mode here.
     */
    setsockopt(INVALID_SOCKET, SOL_SOCKET, SO_OPENTYPE,
	       &optval, sizeof(optval));
#endif

    winsock_inited = 1;
  }
  return 0;
}

static void
shutdownHandler(void)
{
  WSACleanup();
}

void
shutdownWinSock()
{
    if (!winsock_uninited) {
	atexit(shutdownHandler);
	winsock_uninited = 1;
    }
}

#endif
