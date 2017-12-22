{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.Syscall where

#include "HsNet.h"
##include "HsNetDef.h"

import Control.Concurrent.MVar
import Control.Monad (when)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import qualified System.Posix.Internals

#if defined(DOMAIN_SOCKET_SUPPORT)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable(..))
#endif

#if defined(mingw32_HOST_OS)
import qualified Control.Exception as E
import Foreign (FunPtr)
import GHC.Conc (asyncDoProc)
#else
import Foreign.C.Error (getErrno, eINTR, eINPROGRESS)
import GHC.Conc (threadWaitWrite)
import GHC.IO (onException)
#endif

#ifdef HAVE_ACCEPT4
import GHC.Conc (threadWaitRead)
#endif

import Network.Socket.Close
import Network.Socket.Internal
import Network.Socket.Options
import Network.Socket.Types

-- ----------------------------------------------------------------------------
-- On Windows, our sockets are not put in non-blocking mode (non-blocking
-- is not supported for regular file descriptors on Windows, and it would
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
-- Socket types

-- | Smart constructor for constructing a 'Socket'. It should only be
-- called once for every new file descriptor. The caller must make
-- sure that the socket is in non-blocking mode. See
-- 'setNonBlockIfNeeded'.
-- 'mkSocket' should be used intead of 'MkSocket'.
mkSocket :: CInt
         -> Family
         -> SocketType
         -> ProtocolNumber
         -> SocketStatus
         -> IO Socket
mkSocket fd fam sType pNum stat = do
   mStat <- newMVar stat
   withSocketsDo $ return ()
   let sock = MkSocket fd fam sType pNum mStat
##if MIN_VERSION_base(4,6,0)
   _ <- mkWeakMVar mStat $ close sock
##endif
   return sock

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
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
-- >>> sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >>> socketFamily sock
-- AF_INET
-- >>> socketType sock
-- Stream
-- >>> bind sock (addrAddress addr)
-- >>> getSocketName sock
-- 127.0.0.1:5000
socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket      -- Unconnected Socket
socket family stype protocol = do
    c_stype <- packSocketTypeOrThrow "socket" stype
    fd <- throwSocketErrorIfMinus1Retry "Network.Socket.socket" $
                c_socket (packFamily family) c_stype protocol
    setNonBlockIfNeeded fd
    sock <- mkSocket fd family stype protocol NotConnected
#if HAVE_DECL_IPV6_V6ONLY
    -- The default value of the IPv6Only option is platform specific,
    -- so we explicitly set it to 0 to provide a common default.
# if defined(mingw32_HOST_OS)
    -- The IPv6Only option is only supported on Windows Vista and later,
    -- so trying to change it might throw an error.
    when (family == AF_INET6 && (stype == Stream || stype == Datagram)) $
      E.catch (setSocketOption sock IPv6Only 0) $ (\(_ :: E.IOException) -> return ())
# elif !defined(__OpenBSD__)
    when (family == AF_INET6 && (stype == Stream || stype == Datagram)) $
      setSocketOption sock IPv6Only 0 `onException` close sock
# endif
#endif
    return sock

-- | Build a pair of connected socket objects using the given address
-- family, socket type, and protocol number.  Address family, socket
-- type, and protocol number are as for the 'socket' function above.
-- Availability: Unix.
#if defined(DOMAIN_SOCKET_SUPPORT)
socketPair :: Family              -- Family Name (usually AF_INET or AF_INET6)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
socketPair family stype protocol = do
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
    c_stype <- packSocketTypeOrThrow "socketPair" stype
    _rc <- throwSocketErrorIfMinus1Retry "Network.Socket.socketpair" $
                c_socketpair (packFamily family) c_stype protocol fdArr
    [fd1,fd2] <- peekArray 2 fdArr
    s1 <- mkNonBlockingSocket fd1
    s2 <- mkNonBlockingSocket fd2
    return (s1,s2)
  where
    mkNonBlockingSocket fd = do
       setNonBlockIfNeeded fd
       mkSocket fd family stype protocol Connected

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
#endif

-- | Set the socket to nonblocking, if applicable to this platform.
--
-- Depending on the platform this is required when using sockets from file
-- descriptors that are passed in through 'recvFd' or other means.
setNonBlockIfNeeded :: CInt -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD fd True

-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'aNY_PORT' is passed then the system assigns the next available
-- use port.
bind :: Socket    -- Unconnected Socket
           -> SockAddr  -- Address to Bind to
           -> IO ()
bind (MkSocket s _family _stype _protocol sockStatus) addr = do
 modifyMVar_ sockStatus $ \ status -> do
 if status /= NotConnected
  then
   ioError $ userError $
     "Network.Socket.bind: can't bind to socket with status " ++ show status
  else do
   withSockAddr addr $ \p_addr sz -> do
   _status <- throwSocketErrorIfMinus1Retry "Network.Socket.bind" $
     c_bind s p_addr (fromIntegral sz)
   return Bound

-----------------------------------------------------------------------------
-- Connecting a socket

-- | Connect to a remote socket at address.
connect :: Socket    -- Unconnected Socket
        -> SockAddr  -- Socket address stuff
        -> IO ()
connect sock@(MkSocket s _family _stype _protocol sockStatus) addr = withSocketsDo $ do
 modifyMVar_ sockStatus $ \currentStatus -> do
 if currentStatus /= NotConnected && currentStatus /= Bound
  then
    ioError $ userError $
      errLoc ++ ": can't connect to socket with status " ++ show currentStatus
  else do
    withSockAddr addr $ \p_addr sz -> do

    let connectLoop = do
           r <- c_connect s p_addr (fromIntegral sz)
           if r == -1
               then do
#if !(defined(HAVE_WINSOCK2_H))
                   err <- getErrno
                   case () of
                     _ | err == eINTR       -> connectLoop
                     _ | err == eINPROGRESS -> connectBlocked
--                   _ | err == eAGAIN      -> connectBlocked
                     _otherwise             -> throwSocketError errLoc
#else
                   throwSocketError errLoc
#endif
               else return ()

#if !(defined(HAVE_WINSOCK2_H))
        connectBlocked = do
           threadWaitWrite (fromIntegral s)
           err <- getSocketOption sock SoError
           if (err == 0)
                then return ()
                else throwSocketErrorCode errLoc (fromIntegral err)
#endif

    connectLoop
    return Connected
 where
   errLoc = "Network.Socket.connect: " ++ show sock

-----------------------------------------------------------------------------
-- Listen

-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: Socket  -- Connected & Bound Socket
       -> Int     -- Queue Length
       -> IO ()
listen (MkSocket s _family _stype _protocol sockStatus) backlog = do
 modifyMVar_ sockStatus $ \ status -> do
 if status /= Bound
   then
     ioError $ userError $
       "Network.Socket.listen: can't listen on socket with status " ++ show status
   else do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.listen" $
       c_listen s (fromIntegral backlog)
     return Listening

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
accept :: Socket                        -- Queue Socket
       -> IO (Socket,                   -- Readable Socket
              SockAddr)                 -- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- isAcceptable sock
 if not okay
   then
     ioError $ userError $
       "Network.Socket.accept: can't accept socket (" ++
         show (family, stype, protocol) ++ ") with status " ++
         show currentStatus
   else do
     let sz = sizeOfSockAddrByFamily family
     allocaBytes sz $ \ sockaddr -> do
#if defined(mingw32_HOST_OS)
     new_sock <-
        if threaded
           then with (fromIntegral sz) $ \ ptr_len ->
                  throwSocketErrorIfMinus1Retry "Network.Socket.accept" $
                    c_accept_safe s sockaddr ptr_len
           else do
                paramData <- c_newAcceptParams s (fromIntegral sz) sockaddr
                rc        <- asyncDoProc c_acceptDoProc paramData
                new_sock  <- c_acceptNewSock    paramData
                c_free paramData
                when (rc /= 0) $
                     throwSocketErrorCode "Network.Socket.accept" (fromIntegral rc)
                return new_sock
#else
     with (fromIntegral sz) $ \ ptr_len -> do
# ifdef HAVE_ACCEPT4
     new_sock <- throwSocketErrorIfMinus1RetryMayBlock "Network.Socket.accept"
                        (threadWaitRead (fromIntegral s))
                        (c_accept4 s sockaddr ptr_len (#const SOCK_NONBLOCK))
# else
     new_sock <- throwSocketErrorWaitRead sock "Network.Socket.accept"
                        (c_accept s sockaddr ptr_len)
     setNonBlockIfNeeded new_sock
# endif /* HAVE_ACCEPT4 */
#endif
     addr <- peekSockAddr sockaddr
     sock' <- mkSocket new_sock family stype protocol Connected
     return (sock', addr)

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
#ifdef HAVE_ACCEPT4
foreign import CALLCONV unsafe "accept4"
  c_accept4 :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> CInt -> IO CInt
#else
foreign import CALLCONV unsafe "accept"
  c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
#endif
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt

#if defined(mingw32_HOST_OS)
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt

foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "HsNet.h acceptNewSock"
  c_acceptNewSock :: Ptr () -> IO CInt
foreign import ccall unsafe "HsNet.h newAcceptParams"
  c_newAcceptParams :: CInt -> CInt -> Ptr a -> IO (Ptr ())
foreign import ccall unsafe "HsNet.h &acceptDoProc"
  c_acceptDoProc :: FunPtr (Ptr () -> IO Int)
foreign import ccall unsafe "free"
  c_free:: Ptr a -> IO ()
#endif

#if defined(DOMAIN_SOCKET_SUPPORT)
-- sending/receiving ancillary socket data; low-level mechanism
-- for transmitting file descriptors, mainly.
sendFd :: Socket -> CInt -> IO ()
sendFd sock outfd = do
  _ <- throwSocketErrorWaitWrite sock "Network.Socket.sendFd" $ c_sendFd (socketFd sock) outfd
  return ()

-- | Receive a file descriptor over a domain socket. Note that the resulting
-- file descriptor may have to be put into non-blocking mode in order to be
-- used safely. See 'setNonBlockIfNeeded'.
recvFd :: Socket -> IO CInt
recvFd sock = do
  theFd <- throwSocketErrorWaitRead sock "Network.Socket.recvFd" $
               c_recvFd (socketFd sock)
  return theFd

foreign import ccall SAFE_ON_WIN "sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall SAFE_ON_WIN "recvFd" c_recvFd :: CInt -> IO CInt
#endif
