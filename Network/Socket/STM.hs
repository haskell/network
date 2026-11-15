{-# LANGUAGE CPP #-}

-- | Waiting for a socket to become ready, through STM.
--
-- This mirrors @select@\/@epoll@.  On Windows it needs WinIO
-- (@+RTS --io-manager=native@): the wait is an @IOCTL_AFD_POLL@ on the
-- socket, as libuv and others use, which completes on the I/O manager's
-- port.  Under MIO all four throw.  Sockets behind a non-IFS Layered
-- Service Provider, deprecated since Windows 8, are not supported.
--
module Network.Socket.STM (
    -- * Waiting for readiness
    waitReadSocketSTM,
    waitAndCancelReadSocketSTM,
    waitWriteSocketSTM,
    waitAndCancelWriteSocketSTM,
) where

import Control.Concurrent.STM (STM)
import Network.Socket.Types
#if !defined(mingw32_HOST_OS)
import Control.Concurrent (threadWaitReadSTM, threadWaitWriteSTM)
import System.Posix.Types (Fd (..))
#elif __IO_MANAGER_WINIO__ >= 2
import GHC.IO.SubSystem ((<!>))
import Network.Socket.Win32.AfdPoll
#endif

-- | STM action to wait until the socket is ready for reading.
--
--   On Windows this needs WinIO, and throws under MIO.
waitReadSocketSTM :: Socket -> IO (STM ())
waitReadSocketSTM s = fst <$> waitAndCancelReadSocketSTM s

-- | STM action to wait until the socket is ready for reading and STM
--   action to cancel the waiting.
--
--   See 'waitReadSocketSTM'.
waitAndCancelReadSocketSTM :: Socket -> IO (STM (), IO ())
#if defined(mingw32_HOST_OS)
waitAndCancelReadSocketSTM _s =
    noReadiness
# if __IO_MANAGER_WINIO__ >= 2
        <!> afdWaitSTM "waitAndCancelReadSocketSTM" afdRead _s
# endif
  where
    noReadiness =
        ioError $
            userError $
                "waitAndCancelReadSocketSTM: not available under this I/O "
                    ++ "manager; use recvBufWith or "
                    ++ "Network.Socket.ByteString.recvWith instead"
#else
waitAndCancelReadSocketSTM s = withFdSocket s $ threadWaitReadSTM . Fd . fromIntegral
#endif

-- | STM action to wait until the socket is ready for writing.
--
--   On Windows this needs WinIO, and throws under MIO.
waitWriteSocketSTM :: Socket -> IO (STM ())
waitWriteSocketSTM s = fst <$> waitAndCancelWriteSocketSTM s

-- | STM action to wait until the socket is ready for writing and STM
--   action to cancel the waiting.
--
--   See 'waitWriteSocketSTM'.
waitAndCancelWriteSocketSTM :: Socket -> IO (STM (), IO ())
#if defined(mingw32_HOST_OS)
waitAndCancelWriteSocketSTM _s =
    noReadiness
# if __IO_MANAGER_WINIO__ >= 2
        <!> afdWaitSTM "waitAndCancelWriteSocketSTM" afdWrite _s
# endif
  where
    noReadiness =
        ioError $
            userError $
                "waitAndCancelWriteSocketSTM: not available under this I/O "
                    ++ "manager; issue the send instead"
#else
waitAndCancelWriteSocketSTM s = withFdSocket s $ threadWaitWriteSTM . Fd . fromIntegral
#endif
