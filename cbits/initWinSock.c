#include "HsNet.h"
#include "HsFFI.h"

#if defined(HAVE_WINSOCK2_H) && !defined(__CYGWIN__)

static int winsock_inited = 0;
static int winsock_uninited = 0;

/* Initialising WinSock... */
int
initWinSock ()
{
  WORD wVersionRequested;
  WSADATA wsaData;  
  int err;

  if (!winsock_inited) {
    wVersionRequested = MAKEWORD( 2, 2 );

    err = WSAStartup ( wVersionRequested, &wsaData );
    
    if ( err != 0 ) {
       return err;
    }

    if ( LOBYTE( wsaData.wVersion ) != 2 ||
       HIBYTE( wsaData.wVersion ) != 2 ) {
      WSACleanup();
      return (-1);
    }

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
