{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.Syscall where

import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Traversable (for)
import qualified Control.Exception as E
import System.IO.Error (catchIOError)

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
import Network.Socket.Cbits
#endif

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Options
import Network.Socket.Types
#if defined(mingw32_HOST_OS)
import qualified Network.Socket.Win32.Syscall as P
#else
import qualified Network.Socket.Posix.Syscall as P
#endif

-- ----------------------------------------------------------------------------
-- On Windows, as on POSIX, sockets are non-blocking, and a call which would
-- block waits for readiness: through an AFD poll on the I/O manager's
-- completion port, which needs WinIO.

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
socket family stype protocol = withSocketsDo $
    E.bracketOnError create c_close setProp `annotateIOException` (show stype ++ " " ++ show protocol)
  where
    setProp fd = do
        -- Let's ensure that the socket (file descriptor) is closed even on
        -- asynchronous exceptions.
        P.setupSocket fd
        s <- mkSocket fd
        -- This socket is not managed by the IO manager yet.
        -- So, we don't have to call "close" which uses "closeFdWith".
        unsetIPv6Only s
        setDontFragment s
        unsetConnReset s
        return s
    create = do
        let c_stype = modifyFlag $ packSocketType stype
        throwSocketErrorIfMinus1Retry "Network.Socket.socket" $
            c_socket (packFamily family) c_stype protocol
#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    modifyFlag c_stype = c_stype .|. sockNonBlock
#else
    modifyFlag c_stype = c_stype
#endif

#if HAVE_DECL_IPV6_V6ONLY
    unsetIPv6Only s = when (family == AF_INET6 && stype `elem` [Stream, Datagram]) $
# if defined(mingw32_HOST_OS)
      -- The IPv6Only option is only supported on Windows Vista and later,
      -- so trying to change it might throw an error.
      setSocketOption s IPv6Only 0 `catchIOError` \_ -> return ()
# elif defined(openbsd_HOST_OS)
      -- don't change IPv6Only
      return ()
# else
      -- The default value of the IPv6Only option is platform specific,
      -- so we explicitly set it to 0 to provide a common default.
      setSocketOption s IPv6Only 0
# endif
#else
    unsetIPv6Only _ = return ()
#endif

    setDontFragment s = when (family == AF_INET) $
#if HAVE_DECL_IP_DONTFRAG || HAVE_DECL_IP_MTU_DISCOVER
      setSocketOption s DontFragment 1 `catchIOError` \_ -> return ()
#else
      -- do nothing
      return ()
#endif

    unsetConnReset s = when (stype == Datagram) $ reportConnReset s False

-- Whether the socket reports the ICMP unreachable an earlier send of its
-- own caused.  Winsock reports it to the next receive even on a socket
-- which was never connected, where POSIX says nothing of it at all: one
-- reply bouncing off a client which has gone ends a server's receive
-- loop.  'socket' therefore turns it off and 'connect' turns it back on,
-- which is the ECONNREFUSED POSIX gives a connected datagram socket.
--
-- It is advice for datagrams alone and does nothing to any other socket,
-- so the result is not worth looking at.
reportConnReset :: Socket -> Bool -> IO ()
reportConnReset = P.reportConnReset

-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'defaultPort' is passed then the system assigns the next available
-- use port.
bind :: SocketAddress sa => Socket -> sa -> IO ()
bind s sa = withSocketAddress sa $ \p_sa siz -> void $ withFdSocket s $ \fd -> do
  let sz = fromIntegral siz
  throwSocketErrorIfMinus1Retry "Network.Socket.bind" (c_bind fd p_sa sz)

-----------------------------------------------------------------------------
-- Connecting a socket

-- | Connect to a remote socket at address.
connect :: SocketAddress sa => Socket -> sa -> IO ()
connect s sa = void (connectWhenWritable s sa waitWrite :: IO (Either () ()))
  where
    waitWrite act = do
        withFdSocket s $ waitWriteSocket s
        Right <$> act

-- | 'connect' which leaves the wait for a connection in progress to
--   the caller: the argument runs the action once the socket is writable.
connectWhenWritable
    :: SocketAddress sa
    => Socket -> sa -> (IO () -> IO (Either a ())) -> IO (Either a ())
connectWhenWritable s sa whenWritable =
    withSocketsDo $ withSocketAddress sa $ \p_sa sz -> do
        r <- connectLoop s p_sa (fromIntegral sz) whenWritable
        when (isRight r) $ reportConnReset s True
        return r

connectLoop
    :: SocketAddress sa
    => Socket -> Ptr sa -> CInt -> (IO () -> IO (Either a ())) -> IO (Either a ())
connectLoop s p_sa sz whenWritable = withFdSocket s $ \fd -> loop fd
  where
    errLoc = "Network.Socket.connect: " ++ show s
    loop fd = do
       r <- c_connect fd p_sa sz
       if r /= -1 then return (Right ()) else do
           next <- P.connectNext errLoc
           case next of
             P.ConnectRetry -> loop fd
             P.ConnectWait  -> whenWritable connected

    connected = do
       err <- getSocketOption s SoError
       when (err /= 0) $ throwSocketErrorCode errLoc (fromIntegral err)

-----------------------------------------------------------------------------
-- Listen

-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: Socket -> Int -> IO ()
listen s backlog = withFdSocket s listen' `annotateIOException` show s
  where
    listen' fd =
        throwSocketErrorIfMinus1Retry_ "Network.Socket.listen" $
            c_listen fd $ fromIntegral backlog

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
-- On Unix, FD_CLOEXEC is set to the new 'Socket'; on Windows, it is not
-- inherited by child processes.
accept :: SocketAddress sa => Socket -> IO (Socket, sa)
accept listing_sock = runIdentity <$> acceptBy
    (\fd sa sz -> Identity <$> P.acceptFd listing_sock fd sa sz) listing_sock

-- | 'accept' which does not wait: 'Nothing' if no connection is queued.
acceptNoWait :: SocketAddress sa => Socket -> IO (Maybe (Socket, sa))
acceptNoWait = acceptBy P.acceptFdNoWait

acceptBy
    :: (SocketAddress sa, Traversable f)
    => (CSocket -> Ptr sa -> Int -> IO (f (CSocket, Int)))
    -> Socket -> IO (f (Socket, sa))
acceptBy acceptFd listing_sock = withNewSocketAddress $ \new_sa sz ->
    withFdSocket listing_sock $ \listing_fd -> do
        r <- E.bracketOnError (acceptFd listing_fd new_sa sz) (traverse_ (c_close . fst)) $
            traverse $ \(fd, len) -> do
                P.setupAccepted fd
                sock <- mkSocket fd
                return (sock, len)
        for r $ \(new_sock, len) -> (,) new_sock <$> peekSocketAddressLen new_sa len

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CSocket
foreign import CALLCONV unsafe "bind"
  c_bind :: CSocket -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "connect"
  c_connect :: CSocket -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "listen"
  c_listen :: CSocket -> CInt -> IO CInt
