{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Syscall where

import Control.Concurrent.MVar
import Control.Monad (when)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)

#if defined(mingw32_HOST_OS)
import qualified Control.Exception as E
import Foreign (FunPtr)
import GHC.Conc (asyncDoProc)
#else
import Foreign.C.Error (getErrno, eINTR, eINPROGRESS)
import GHC.Conc (threadWaitWrite)
import GHC.IO (onException)
#endif

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
import Data.Bits ((.|.))
import GHC.Conc (threadWaitRead)
#endif

import Network.Socket.Close
import Network.Socket.Fcntl
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

-- | Creating 'Socket' from the a low level socket (i.e. a file descriptor).
--   The caller must make sure that the low level socket is
--   in non-blocking mode. See 'setNonBlockIfNeeded'.
mkSocket :: CInt
         -> Family
         -> SocketType
         -> ProtocolNumber
         -> SocketStatus
         -> IO Socket
mkSocket fd fam sType pNum stat = do
   mStat <- newMVar stat
   withSocketsDo $ return ()
   let sock = Socket fd fam sType pNum mStat
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
#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    fd <- throwSocketErrorIfMinus1Retry "Network.Socket.socket" $
                c_socket (packFamily family) (c_stype .|. (#const SOCK_NONBLOCK)) protocol
#else
    fd <- throwSocketErrorIfMinus1Retry "Network.Socket.socket" $
                c_socket (packFamily family) c_stype protocol
    setNonBlockIfNeeded fd
#endif
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

-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'defaultPort' is passed then the system assigns the next available
-- use port.
bind :: Socket    -- Unconnected Socket
           -> SockAddr  -- Address to Bind to
           -> IO ()
bind Socket{..} addr =
 modifyMVar_ socketStatus $ \status ->
   if status /= NotConnected
    then
     ioError $ userError $
       "Network.Socket.bind: can't bind to socket with status " ++ show status
    else
     withSockAddr addr $ \p_addr sz -> do
       _status <- throwSocketErrorIfMinus1Retry "Network.Socket.bind" $
         c_bind socketFd p_addr (fromIntegral sz)
       return Bound

-----------------------------------------------------------------------------
-- Connecting a socket

-- | Connect to a remote socket at address.
connect :: Socket    -- Unconnected Socket
        -> SockAddr  -- Socket address stuff
        -> IO ()
connect sock@Socket{..} addr = withSocketsDo $
 modifyMVar_ socketStatus $ \currentStatus ->
 if currentStatus /= NotConnected && currentStatus /= Bound
  then
    ioError $ userError $
      errLoc ++ ": can't connect to socket with status " ++ show currentStatus
  else
    withSockAddr addr $ \p_addr sz -> do

    let connectLoop = do
           r <- c_connect socketFd p_addr (fromIntegral sz)
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
           threadWaitWrite (fromIntegral socketFd)
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
listen Socket{..} backlog =
 modifyMVar_ socketStatus $ \status ->
   if status /= Bound
     then
       ioError $ userError $
         "Network.Socket.listen: can't listen on socket with status " ++ show status
     else do
       throwSocketErrorIfMinus1Retry_ "Network.Socket.listen" $
         c_listen socketFd (fromIntegral backlog)
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

accept sock@Socket{..} = do
 currentStatus <- readMVar socketStatus
 okay <- isAcceptable sock
 if not okay
   then
     ioError $ userError $
       "Network.Socket.accept: can't accept socket (" ++
         show (socketFamily, socketType, socketProtocol) ++ ") with status " ++
         show currentStatus
   else do
     let sz = sizeOfSockAddrByFamily socketFamily
     allocaBytes sz $ \ sockaddr -> do
#if defined(mingw32_HOST_OS)
     new_fd <-
        if threaded
           then with (fromIntegral sz) $ \ ptr_len ->
                  throwSocketErrorIfMinus1Retry "Network.Socket.accept" $
                    c_accept_safe socketFd sockaddr ptr_len
           else do
                paramData <- c_newAcceptParams socketFd (fromIntegral sz) sockaddr
                rc        <- asyncDoProc c_acceptDoProc paramData
                new_fd  <- c_acceptNewSock    paramData
                c_free paramData
                when (rc /= 0) $
                     throwSocketErrorCode "Network.Socket.accept" (fromIntegral rc)
                return new_fd
#else
     with (fromIntegral sz) $ \ ptr_len -> do
# ifdef HAVE_ADVANCED_SOCKET_FLAGS
     new_fd <- throwSocketErrorIfMinus1RetryMayBlock "Network.Socket.accept"
                        (threadWaitRead (fromIntegral socketFd))
                        (c_accept4 socketFd sockaddr ptr_len ((#const SOCK_NONBLOCK) .|. (#const SOCK_CLOEXEC)))
# else
     new_fd <- throwSocketErrorWaitRead sock "Network.Socket.accept"
                        (c_accept socketFd sockaddr ptr_len)
     setNonBlockIfNeeded new_fd
     setCloseOnExecIfNeeded new_fd
# endif /* HAVE_ADVANCED_SOCKET_FLAGS */
#endif
     addr <- peekSockAddr sockaddr
     sock' <- mkSocket new_fd socketFamily socketType socketProtocol Connected
     return (sock', addr)

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
#ifdef HAVE_ADVANCED_SOCKET_FLAGS
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
