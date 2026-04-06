{-# LANGUAGE CPP #-}

module Network.Socket.Handle where

import qualified GHC.IO.Device (IODeviceType (Stream))
import GHC.IO.Handle.FD (fdToHandle')
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering)

#if defined(mingw32_HOST_OS) && defined(HAS_WINIO)
import Foreign.Ptr (wordPtrToPtr)
import GHC.IO.SubSystem ((<!>))
import qualified GHC.Event.Windows as Mgr
import GHC.IO.Windows.Handle (fromHANDLE, Io, NativeHandle)
import GHC.IO.Handle.Windows (mkHandleFromHANDLE)
#endif

import Network.Socket.Types

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
socketToHandle s mode = invalidateSocket s err $ \oldfd -> do
    h <-
#if defined(mingw32_HOST_OS) && defined(HAS_WINIO)
        socketToHandleMIO oldfd <!> socketToHandleWinIO oldfd
#else
        socketToHandleMIO oldfd
#endif
    hSetBuffering h NoBuffering
    return h
  where
    err _ = ioError $ userError $ "socketToHandle: socket is no longer valid"

    socketToHandleMIO oldfd =
        fdToHandle'
            (fromIntegral oldfd)
            (Just GHC.IO.Device.Stream)
            True
            (show s)
            mode
            True {-bin-}

#if defined(mingw32_HOST_OS) && defined(HAS_WINIO)
    socketToHandleWinIO oldfd = do
        let hwnd = wordPtrToPtr $ fromIntegral oldfd
        Mgr.associateHandle' hwnd
        let nativeHwnd = fromHANDLE hwnd :: Io NativeHandle
        mkHandleFromHANDLE nativeHwnd GHC.IO.Device.Stream (show s) mode Nothing
#endif
