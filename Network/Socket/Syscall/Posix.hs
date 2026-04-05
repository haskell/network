{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.Syscall.Posix where

import Foreign.Marshal.Utils (with)
import qualified Control.Exception as E
# if defined(mingw32_HOST_OS)
import System.IO.Error (catchIOError)
#endif

#if defined(mingw32_HOST_OS)
import Control.Exception (bracket)
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
import Network.Socket.Types (
    Family(..),
    SocketAddress,
    SocketType(..),
    ProtocolNumber,
    withSocketAddress,
    withNewSocketAddress,
    peekSocketAddress
    )
import qualified Network.Socket.Types as Generic
import Network.Socket.Types.Posix

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

socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket      -- Unconnected Socket
socket family stype protocol = E.bracketOnError create c_close $ \fd -> do
    -- Let's ensure that the socket (file descriptor) is closed even on
    -- asynchronous exceptions.
    setNonBlock fd
    s <- mkSocket fd
    -- This socket is not managed by the IO manager yet.
    -- So, we don't have to call "close" which uses "closeFdWith".
    unsetIPv6Only s
    setDontFragment s
    return s
  where
    create = do
        let c_stype = modifyFlag $ packSocketType stype
        throwSocketErrorIfMinus1Retry "Network.Socket.socket" $
            c_socket (packFamily family) c_stype protocol

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    modifyFlag c_stype = c_stype .|. sockNonBlock
#else
    modifyFlag c_stype = c_stype
#endif

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    setNonBlock _ = return ()
#else
    setNonBlock fd = setNonBlockIfNeeded fd
#endif

#if HAVE_DECL_IPV6_V6ONLY
    unsetIPv6Only s = when (family == AF_INET6 && stype `elem` [Stream, Datagram]) $
# if defined(mingw32_HOST_OS)
      -- The IPv6Only option is only supported on Windows Vista and later,
      -- so trying to change it might throw an error.
      setSocketOption (Generic.Socket $ Left s) IPv6Only 0 `catchIOError` \_ -> return ()
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
# if defined(mingw32_HOST_OS)
      setSocketOption (Generic.Socket $ Left s) DontFragment 1
# else
      setSocketOption s DontFragment 1
# endif
#else
      -- do nothing
      return ()
#endif


bind :: SocketAddress sa => Socket -> sa -> IO ()
bind s sa = withSocketAddress sa $ \p_sa siz -> void $ withFdSocket s $ \fd -> do
  let sz = fromIntegral siz
  throwSocketErrorIfMinus1Retry "Network.Socket.bind" $ c_bind fd p_sa sz

connect :: SocketAddress sa => Socket -> sa -> IO ()
connect s sa = withSocketsDo $ withSocketAddress sa $ \p_sa sz ->
    connectLoop s p_sa (fromIntegral sz)

connectLoop :: SocketAddress sa => Socket -> Ptr sa -> CInt -> IO ()
connectLoop s p_sa sz = withFdSocket s $ \fd -> loop fd
  where
    errLoc = "Network.Socket.connect: " ++ show s
    loop fd = do
       r <- c_connect fd p_sa sz
       when (r == -1) $ do
#if defined(mingw32_HOST_OS)
           throwSocketError errLoc
#else
           err <- getErrno
           case () of
             _ | err == eINTR       -> loop fd
             _ | err == eINPROGRESS -> connectBlocked
--           _ | err == eAGAIN      -> connectBlocked
             _otherwise             -> throwSocketError errLoc

    connectBlocked = do
       withFdSocket s $ threadWaitWrite . fromIntegral
       err <- getSocketOption s SoError
       when (err /= 0) $ throwSocketErrorCode errLoc (fromIntegral err)
#endif


listen :: Socket -> Int -> IO ()
listen s backlog = withFdSocket s $ \fd -> do
    throwSocketErrorIfMinus1Retry_ "Network.Socket.listen" $
        c_listen fd $ fromIntegral backlog


accept :: SocketAddress sa => Socket -> IO (Socket, sa)
accept listing_sock = withNewSocketAddress $ \new_sa sz ->
    withFdSocket listing_sock $ \listing_fd -> do
 new_sock <- E.bracketOnError (callAccept listing_fd new_sa sz) c_close mkSocket
 new_addr <- peekSocketAddress new_sa
 return (new_sock, new_addr)
  where
#if defined(mingw32_HOST_OS)
     callAccept fd sa sz
       | threaded  = with (fromIntegral sz) $ \ ptr_len ->
                       throwSocketErrorIfMinus1Retry "Network.Socket.accept" $
                         c_accept_safe fd sa ptr_len
       | otherwise = do
             bracket (c_newAcceptParams fd (fromIntegral sz) sa) c_free $ \paramData -> do
                 rc     <- asyncDoProc c_acceptDoProc paramData
                 new_fd <- c_acceptNewSock paramData
                 when (rc /= 0) $
                     throwSocketErrorCode "Network.Socket.accept" (fromIntegral rc)
                 return new_fd
#else
     callAccept fd sa sz = with (fromIntegral sz) $ \ ptr_len -> do
# ifdef HAVE_ADVANCED_SOCKET_FLAGS
       throwSocketErrorWaitRead listing_sock "Network.Socket.accept"
                        (c_accept4 fd sa ptr_len (sockNonBlock .|. sockCloexec))
# else
       new_fd <- throwSocketErrorWaitRead listing_sock "Network.Socket.accept"
                        (c_accept fd sa ptr_len)
       setNonBlockIfNeeded new_fd
       setCloseOnExecIfNeeded new_fd
       return new_fd
# endif /* HAVE_ADVANCED_SOCKET_FLAGS */
#endif

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "connect"
  c_connect :: CInt -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
foreign import CALLCONV unsafe "accept4"
  c_accept4 :: CInt -> Ptr sa -> Ptr CInt{-CSockLen???-} -> CInt -> IO CInt
#else
foreign import CALLCONV unsafe "accept"
  c_accept :: CInt -> Ptr sa -> Ptr CInt{-CSockLen???-} -> IO CInt
#endif

#if defined(mingw32_HOST_OS)
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr sa -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import ccall unsafe "rtsSupportsBoundThreads"
  threaded :: Bool
foreign import ccall unsafe "HsNet.h acceptNewSock"
  c_acceptNewSock :: Ptr () -> IO CInt
foreign import ccall unsafe "HsNet.h newAcceptParams"
  c_newAcceptParams :: CInt -> CInt -> Ptr a -> IO (Ptr ())
foreign import ccall unsafe "HsNet.h &acceptDoProc"
  c_acceptDoProc :: FunPtr (Ptr () -> IO Int)
foreign import ccall unsafe "free"
  c_free:: Ptr a -> IO ()
#endif
