{-# LANGUAGE CPP #-}
module Network.Socket.Handle (socketToHandle) where

import qualified GHC.IO.Device (IODeviceType (Stream))
import GHC.IO.Handle.FD (fdToHandle')
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering)

#if defined(mingw32_HOST_OS)
import GHC.IO.Windows.Handle
import GHC.IO.Handle.Windows
import qualified Network.Socket.Types.WinIO as Win
#endif

import Network.Socket.Types
import qualified Network.Socket.Types.Posix as Posix

-- | Turns a Socket into an 'Handle'. By default, the new handle is
-- unbuffered. Use 'System.IO.hSetBuffering' to change the buffering.
--
-- Note that since a 'Handle' is automatically closed by a finalizer
-- when it is no longer referenced, you should avoid doing any more
-- operations on the 'Socket' after calling 'socketToHandle'.  To
-- close the 'Socket' after 'socketToHandle', call 'System.IO.hClose'
-- on the 'Handle'.
--
-- Caveat 'Handle' is not recommended for network programming in
-- Haskell, e.g. merely performing 'hClose' on a TCP socket won't
-- cooperate with peer's 'gracefulClose', i.e. proper shutdown
-- sequence with appropriate handshakes specified by the protocol.
socketToHandle :: Socket -> IOMode -> IO Handle
#if defined(mingw32_HOST_OS)
socketToHandle = eitherSocket socketToHandlePosix socketToHandleWindows

socketToHandleWindows :: Win.Socket -> IOMode -> IO Handle
socketToHandleWindows s mode = Win.invalidateSocket s err $ \oldsock -> do
    h <- mkHandleFromHANDLE (fromHANDLE oldsock :: Io NativeHandle) GHC.IO.Device.Stream "socket" mode Nothing
    hSetBuffering h NoBuffering
    return h
  where
    err _ = ioError $ userError $ "socketToHandle: socket is no longer valid"
#else
socketToHandle = socketToHandlePosix
#endif

socketToHandlePosix :: Posix.Socket -> IOMode -> IO Handle
socketToHandlePosix s mode = Posix.invalidateSocket s err $ \oldfd -> do
    h <- fdToHandle' oldfd (Just GHC.IO.Device.Stream) True (show s) mode True {-bin-}
    hSetBuffering h NoBuffering
    return h
  where
    err _ = ioError $ userError $ "socketToHandle: socket is no longer valid"
