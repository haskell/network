#include "HsNet.h"
#include "HsFFI.h"

#if defined(_WIN32)

/* to the end */

/* NULL for a code not in the table, which the caller describes itself:
   formatting it here would need a static buffer, shared between threads. */
const char*
getWSErrorDescr(int err)
{
  switch (err) {
  case WSAEINTR:  return "Interrupted function call (WSAEINTR)";
  case WSAEBADF:  return "bad socket descriptor (WSAEBADF)";
  case WSAEACCES: return "Permission denied (WSAEACCES)";
  case WSAEFAULT: return "Bad address (WSAEFAULT)";
  case WSAEINVAL: return "Invalid argument (WSAEINVAL)";
  case WSAEMFILE: return "Too many open files (WSAEMFILE)";
  case WSAEWOULDBLOCK:  return "Resource temporarily unavailable (WSAEWOULDBLOCK)";
  case WSAEINPROGRESS:  return "Operation now in progress (WSAEINPROGRESS)";
  case WSAEALREADY:     return "Operation already in progress (WSAEALREADY)";
  case WSAENOTSOCK:     return "Socket operation on non-socket (WSAENOTSOCK)";
  case WSAEDESTADDRREQ: return "Destination address required (WSAEDESTADDRREQ)";
  case WSAEMSGSIZE:    	return "Message too long (WSAEMSGSIZE)";
  case WSAEPROTOTYPE:   return "Protocol wrong type for socket (WSAEPROTOTYPE)";
  case WSAENOPROTOOPT:  return "Bad protocol option (WSAENOPROTOOPT)";
  case WSAEPROTONOSUPPORT: return "Protocol not supported (WSAEPROTONOSUPPORT)";
  case WSAESOCKTNOSUPPORT: return "Socket type not supported (WSAESOCKTNOSUPPORT)";
  case WSAEOPNOTSUPP:      return "Operation not supported (WSAEOPNOTSUPP)";
  case WSAEPFNOSUPPORT:    return "Protocol family not supported (WSAEPFNOSUPPORT)";
  case WSAEAFNOSUPPORT:    return "Address family not supported by protocol family (WSAEAFNOSUPPORT)";
  case WSAEADDRINUSE:      return "Address already in use (WSAEADDRINUSE)";
  case WSAEADDRNOTAVAIL:   return "Cannot assign requested address (WSAEADDRNOTAVAIL)";
  case WSAENETDOWN:        return "Network is down (WSAENETDOWN)";
  case WSAENETUNREACH:     return "Network is unreachable (WSAENETUNREACH)";
  case WSAENETRESET:       return "Network dropped connection on reset (WSAENETRESET)";
  case WSAECONNABORTED:    return "Software caused connection abort (WSAECONNABORTED)";
  case WSAECONNRESET:      return "Connection reset by peer (WSAECONNRESET)";
  case WSAENOBUFS:         return "No buffer space available (WSAENOBUFS)";
  case WSAEISCONN:         return "Socket is already connected (WSAEISCONN)";
  case WSAENOTCONN:        return "Socket is not connected (WSAENOTCONN)";
  case WSAESHUTDOWN:       return "Cannot send after socket shutdown (WSAESHUTDOWN)";
  case WSAETOOMANYREFS:    return "Too many references (WSAETOOMANYREFS)";
  case WSAETIMEDOUT:       return "Connection timed out (WSAETIMEDOUT)";
  case WSAECONNREFUSED:    return "Connection refused (WSAECONNREFUSED)";
  case WSAELOOP:           return "Too many levels of symbolic links (WSAELOOP)";
  case WSAENAMETOOLONG:    return "Filename too long (WSAENAMETOOLONG)";
  case WSAEHOSTDOWN:       return "Host is down (WSAEHOSTDOWN)";
  case WSAEHOSTUNREACH:    return "Host is unreachable (WSAEHOSTUNREACH)";
  case WSAENOTEMPTY:       return "Resource not empty (WSAENOTEMPTY)";
  case WSAEPROCLIM:        return "Too many processes (WSAEPROCLIM)";
  case WSAEUSERS:          return "Too many users (WSAEUSERS)";
  case WSAEDQUOT:          return "Disk quota exceeded (WSAEDQUOT)";
  case WSAESTALE:          return "Stale NFS file handle (WSAESTALE)";
  case WSAEREMOTE:         return "Too many levels of remote in path (WSAEREMOTE)";
  case WSAEDISCON:         return "Graceful shutdown in progress (WSAEDISCON)";
  case WSASYSNOTREADY:     return "Network subsystem is unavailable (WSASYSNOTREADY)";
  case WSAVERNOTSUPPORTED: return "Winsock.dll version out of range (WSAVERNOTSUPPORTED)";
  case WSANOTINITIALISED:  return "Successful WSAStartup not yet performed (WSANOTINITIALISED)";
#ifdef WSATYPE_NOT_FOUND
  case WSATYPE_NOT_FOUND:  return "Class type not found (WSATYPE_NOT_FOUND)";
#endif
  case WSAHOST_NOT_FOUND:  return "Host not found (WSAHOST_NOT_FOUND)";
  case WSATRY_AGAIN:       return "Nonauthoritative host not found (WSATRY_AGAIN)";
  case WSANO_RECOVERY:     return "This is a nonrecoverable error (WSANO_RECOVERY)";
  case WSANO_DATA:         return "Valid name, no data record of requested type (WSANO_DATA)";
  default:
    return NULL;
  }
}

/* A completion status as a Winsock code.  The I/O manager hands over the
   NTSTATUS of an operation which completed through the port, and the
   Winsock code of one which failed at once; the latter stays as it is. */
int
hsnet_winsock_code(unsigned long st)
{
  if (st < 0x80000000UL) return (int)st;
  switch (st) {
  case 0x80000005UL: return WSAEMSGSIZE;        /* STATUS_BUFFER_OVERFLOW */
  case 0x80000011UL: return WSAEWOULDBLOCK;     /* STATUS_DEVICE_BUSY */
  case 0xC0000005UL: return WSAEFAULT;          /* STATUS_ACCESS_VIOLATION */
  case 0xC0000008UL: return WSAENOTSOCK;        /* STATUS_INVALID_HANDLE */
  case 0xC000000DUL: return WSAEINVAL;          /* STATUS_INVALID_PARAMETER */
  case 0xC0000011UL: return WSAEDISCON;         /* STATUS_END_OF_FILE */
  case 0xC0000017UL: return WSAENOBUFS;         /* STATUS_NO_MEMORY */
  case 0xC0000022UL: return WSAEACCES;          /* STATUS_ACCESS_DENIED */
  case 0xC0000024UL: return WSAENOTSOCK;        /* STATUS_OBJECT_TYPE_MISMATCH */
  case 0xC000009AUL: return WSAENOBUFS;         /* STATUS_INSUFFICIENT_RESOURCES */
  case 0xC00000B5UL: return WSAETIMEDOUT;       /* STATUS_IO_TIMEOUT */
  case 0xC0000120UL: return WSA_OPERATION_ABORTED; /* STATUS_CANCELLED */
  case 0xC000013BUL: return WSAECONNABORTED;    /* STATUS_LOCAL_DISCONNECT */
  case 0xC000013CUL: return WSAECONNRESET;      /* STATUS_REMOTE_DISCONNECT */
  case 0xC000020CUL: return WSAECONNRESET;      /* STATUS_CONNECTION_DISCONNECTED */
  case 0xC000020DUL: return WSAECONNRESET;      /* STATUS_CONNECTION_RESET */
  case 0xC0000236UL: return WSAECONNREFUSED;    /* STATUS_CONNECTION_REFUSED */
  case 0xC0000241UL: return WSAECONNABORTED;    /* STATUS_CONNECTION_ABORTED */
  case 0xC000023CUL: return WSAENETUNREACH;     /* STATUS_NETWORK_UNREACHABLE */
  case 0xC000023DUL: return WSAEHOSTUNREACH;    /* STATUS_HOST_UNREACHABLE */
  case 0xC000023EUL: return WSAENETUNREACH;     /* STATUS_PROTOCOL_UNREACHABLE */
  case 0xC000023FUL: return WSAECONNRESET;      /* STATUS_PORT_UNREACHABLE */
  default:           return (int)st;
  }
}

#endif

