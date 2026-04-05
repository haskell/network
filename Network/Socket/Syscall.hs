{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNetDef.h"

module Network.Socket.Syscall where

import Data.Bifunctor
import Foreign.Marshal.Utils (with)
import qualified Control.Exception as E
# if defined(mingw32_HOST_OS)
import System.IO.Error (catchIOError)
#endif

#if defined(mingw32_HOST_OS)
import Control.Exception (bracket)
import Foreign (FunPtr)
import GHC.Conc (asyncDoProc)
#else
import Foreign.C.Error (getErrno, eINTR, eINPROGRESS)
import GHC.Conc (threadWaitWrite)
#endif

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
import Network.Socket.Cbits
#else
import Network.Socket.Fcntl
#endif

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Options
import Network.Socket.Types
import qualified Network.Socket.Types.Posix as Posix

import qualified Network.Socket.Syscall.Posix as Posix
#if defined(mingw32_HOST_OS)
import qualified Network.Socket.Types.WinIO as Win
import qualified Network.Socket.Syscall.WinIO as Win
import GHC.IO.SubSystem
#endif

-- ----------------------------------------------------------------------------
-- In the Posix event manager, our sockets are not put in non-blocking mode
-- (non-blocking for regular file descriptors on Windows in the PEM, and it would
-- be a pain to support it only for sockets).  So there are two cases:
--
--  - the threaded RTS uses safe calls for socket operations to get
--    non-blocking I/O, just like the rest of the I/O library
--
--  - with the non-threaded RTS, only some operations on sockets will be
--    non-blocking.  Reads and writes go through the normal async I/O
--    system.  accept() uses asyncDoProc so is non-blocking.  A handful
--    of others (recvFrom, sendFd, recvFd) will block all threads - if this
--    is a problem, -threaded is the workaround.
--

-----------------------------------------------------------------------------
-- Connection Functions

-- In the following connection and binding primitives.  The names of
-- the equivalent C functions have been preserved where possible. It
-- should be noted that some of these names used in the C library,
-- \tr{bind} in particular, have a different meaning to many Haskell
-- programmers and have thus been renamed by appending the prefix
-- Socket.

-- | Create a new socket using the given address family, socket type
-- and protocol number.  The address family is usually 'AF_INET',
-- 'AF_INET6', or 'AF_UNIX'.  The socket type is usually 'Stream' or
-- 'Datagram'.  The protocol number is usually 'defaultProtocol'.
-- If 'AF_INET6' is used and the socket type is 'Stream' or 'Datagram',
-- the 'IPv6Only' socket option is set to 0 so that both IPv4 and IPv6
-- can be handled with one socket.
--
-- >>> import Network.Socket
-- >>> import qualified Data.List.NonEmpty as NE
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
-- >>> addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
-- >>> sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >>> Network.Socket.bind sock (addrAddress addr)
-- >>> getSocketName sock
-- 127.0.0.1:5000
socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket      -- Unconnected Socket
#if defined(mingw32_HOST_OS)
socket fam st p = Socket <$>
    fmap Left (Posix.socket fam st p) <!> fmap Right (Win.socket fam st p)
#else
socket = Posix.socket
#endif


-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'defaultPort' is passed then the system assigns the next available
-- use port.
bind :: SocketAddress sa => Socket -> sa -> IO ()
#if defined(mingw32_HOST_OS)
bind = eitherSocket Posix.bind Win.bind
#else
bind = Posix.bind
#endif

-----------------------------------------------------------------------------
-- Connecting a socket

-- | Connect to a remote socket at address.
connect :: Socket -> SockAddr -> IO ()
#if defined(mingw32_HOST_OS)
connect = eitherSocket Posix.connect Win.connect
#else
connect = Posix.connect
#endif

-----------------------------------------------------------------------------
-- Listen

-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: Socket -> Int -> IO ()
#if defined(mingw32_HOST_OS)
listen = eitherSocket Posix.listen Win.listen
#else
listen = Posix.listen
#endif

-----------------------------------------------------------------------------
-- Accept
--
-- A call to `accept' only returns when data is available on the given
-- socket, unless the socket has been set to non-blocking.  It will
-- return a new socket which should be used to read the incoming data and
-- should then be closed. Using the socket returned by `accept' allows
-- incoming requests to be queued on the original socket.

-- | Accept a connection.  The socket must be bound to an address and
-- listening for connections.  The return value is a pair @(conn,
-- address)@ where @conn@ is a new socket object usable to send and
-- receive data on the connection, and @address@ is the address bound
-- to the socket on the other end of the connection.
-- On Unix, FD_CLOEXEC is set to the new 'Socket'.
accept :: SocketAddress sa => Socket -> IO (Socket, sa)
#if defined(mingw32_HOST_OS)
accept (Socket s) = case s of
    Left (p :: Posix.Socket) -> first (Socket . Left) <$> Posix.accept p
    Right (w :: Win.Socket) -> first (Socket . Right) <$> Win.accept w
#else
accept = Posix.accept
#endif
