module Network.Socket.Cbits where

#include "HsNet.h"

import Network.Socket.Imports
#if defined(mingw32_HOST_OS)
import GHC.IO.Exception (IOErrorType (..))
#endif

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = #const SOMAXCONN

#if defined(mingw32_HOST_OS)
wsaNotInitialized :: CInt
wsaNotInitialized = #const WSANOTINITIALISED
-- | Whether a datagram socket reports an ICMP unreachable which came
--   back from an earlier send.  Winsock turns it on by default.
sioUDPConnReset :: Word32
sioUDPConnReset = #const SIO_UDP_CONNRESET
-- | What closesocket gives a call already blocked on the socket.
wsaEINTR :: CInt
wsaEINTR = #const WSAEINTR
-- | What a socket operation gives once the socket has been closed.
wsaENOTSOCK :: CInt
wsaENOTSOCK = #const WSAENOTSOCK
-- | What accept() gives for a connection the peer gave up on.  Winsock
--   names WSAECONNRESET where accept(2) names ECONNABORTED.
wsaECONNABORTED :: CInt
wsaECONNABORTED = #const WSAECONNABORTED
wsaECONNRESET :: CInt
wsaECONNRESET = #const WSAECONNRESET
wsaENETDOWN :: CInt
wsaENETDOWN = #const WSAENETDOWN
wsaENETRESET :: CInt
wsaENETRESET = #const WSAENETRESET
wsaENETUNREACH :: CInt
wsaENETUNREACH = #const WSAENETUNREACH
wsaEHOSTDOWN :: CInt
wsaEHOSTDOWN = #const WSAEHOSTDOWN
wsaEHOSTUNREACH :: CInt
wsaEHOSTUNREACH = #const WSAEHOSTUNREACH
wsaENOPROTOOPT :: CInt
wsaENOPROTOOPT = #const WSAENOPROTOOPT
-- | The 'IOErrorType' which 'Foreign.C.Error.errnoToIOError' gives the
--   errno of the same name.  WSAEINTR stays 'OtherError': on Windows it
--   is what closesocket gives a blocked call, not an interrupt.
winsockErrorType :: CInt -> IOErrorType
winsockErrorType rc = case rc of
    #{const WSAEACCES} -> PermissionDenied
    #{const WSAEADDRINUSE} -> ResourceBusy
    #{const WSAEADDRNOTAVAIL} -> UnsupportedOperation
    #{const WSAEAFNOSUPPORT} -> UnsupportedOperation
    #{const WSAEALREADY} -> AlreadyExists
    #{const WSAEBADF} -> InvalidArgument
    #{const WSAECONNREFUSED} -> NoSuchThing
    #{const WSAECONNRESET} -> ResourceVanished
    #{const WSAEDESTADDRREQ} -> InvalidArgument
    #{const WSAEDQUOT} -> PermissionDenied
    #{const WSAEHOSTDOWN} -> NoSuchThing
    #{const WSAEHOSTUNREACH} -> NoSuchThing
    #{const WSAEINPROGRESS} -> AlreadyExists
    #{const WSAEINVAL} -> InvalidArgument
    #{const WSAEISCONN} -> AlreadyExists
    #{const WSAELOOP} -> InvalidArgument
    #{const WSAEMFILE} -> ResourceExhausted
    #{const WSAEMSGSIZE} -> ResourceExhausted
    #{const WSAENAMETOOLONG} -> InvalidArgument
    #{const WSAENETDOWN} -> ResourceVanished
    #{const WSAENETRESET} -> ResourceVanished
    #{const WSAENETUNREACH} -> NoSuchThing
    #{const WSAENOBUFS} -> ResourceExhausted
    #{const WSAENOPROTOOPT} -> UnsupportedOperation
    #{const WSAENOTCONN} -> InvalidArgument
    #{const WSAENOTEMPTY} -> UnsatisfiedConstraints
    #{const WSAENOTSOCK} -> InvalidArgument
    #{const WSAEOPNOTSUPP} -> UnsupportedOperation
    #{const WSAEPFNOSUPPORT} -> UnsupportedOperation
    #{const WSAEPROCLIM} -> PermissionDenied
    #{const WSAEPROTONOSUPPORT} -> ProtocolError
    #{const WSAEPROTOTYPE} -> ProtocolError
    #{const WSAEREMOTE} -> IllegalOperation
    #{const WSAESHUTDOWN} -> IllegalOperation
    #{const WSAESOCKTNOSUPPORT} -> UnsupportedOperation
    #{const WSAESTALE} -> ResourceVanished
    #{const WSAETIMEDOUT} -> TimeExpired
    #{const WSAETOOMANYREFS} -> ResourceExhausted
    #{const WSAEUSERS} -> ResourceExhausted
    _ -> OtherError
#else
fGetFd :: CInt
fGetFd = #const F_GETFD
fGetFl :: CInt
fGetFl = #const F_GETFL
fdCloexec :: CInt
fdCloexec = #const FD_CLOEXEC
oNonBlock :: CInt
oNonBlock = #const O_NONBLOCK
# if defined(HAVE_ADVANCED_SOCKET_FLAGS)
sockNonBlock :: CInt
sockNonBlock = #const SOCK_NONBLOCK
sockCloexec :: CInt
sockCloexec = #const SOCK_CLOEXEC
# endif
#endif
