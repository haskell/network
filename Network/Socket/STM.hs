{-# LANGUAGE CPP #-}

-- | Waiting for a socket to become ready, through STM.
--
-- This mirrors @select@\/@epoll@.  On Windows the wait is an
-- @IOCTL_AFD_POLL@ on the socket, as libuv and others use, which
-- completes on the I/O manager's port.  Sockets behind a non-IFS Layered
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
#else
import Network.Socket.Win32.AfdPoll
#endif

-- | STM action to wait until the socket is ready for reading.
waitReadSocketSTM :: Socket -> IO (STM ())
waitReadSocketSTM s = fst <$> waitAndCancelReadSocketSTM s

-- | STM action to wait until the socket is ready for reading and STM
--   action to cancel the waiting.
--
--   See 'waitReadSocketSTM'.
waitAndCancelReadSocketSTM :: Socket -> IO (STM (), IO ())
#if defined(mingw32_HOST_OS)
waitAndCancelReadSocketSTM = afdWaitSTM "waitAndCancelReadSocketSTM" afdRead
#else
waitAndCancelReadSocketSTM s = withFdSocket s $ threadWaitReadSTM . Fd . fromIntegral
#endif

-- | STM action to wait until the socket is ready for writing.
waitWriteSocketSTM :: Socket -> IO (STM ())
waitWriteSocketSTM s = fst <$> waitAndCancelWriteSocketSTM s

-- | STM action to wait until the socket is ready for writing and STM
--   action to cancel the waiting.
--
--   See 'waitWriteSocketSTM'.
waitAndCancelWriteSocketSTM :: Socket -> IO (STM (), IO ())
#if defined(mingw32_HOST_OS)
waitAndCancelWriteSocketSTM = afdWaitSTM "waitAndCancelWriteSocketSTM" afdWrite
#else
waitAndCancelWriteSocketSTM s = withFdSocket s $ threadWaitWriteSTM . Fd . fromIntegral
#endif
