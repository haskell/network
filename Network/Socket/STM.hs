{-# LANGUAGE CPP #-}

-- | Waiting for a socket to become ready, through STM.
--
-- This mirrors @select@\/@epoll@ and is __POSIX only__: Windows
-- completion ports report that an operation has finished, not that one
-- could be started, so there is no readiness to wait for.  All four
-- throw there.  What runs on Windows instead is
-- "Network.Socket.Win32.Cancel", through 'Network.Socket.withReadable'.
--
module Network.Socket.STM (
    -- * Waiting for readiness (POSIX only)
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
#endif

-- | STM action to wait until the socket is ready for reading.
--
--   __POSIX only.__  On Windows this throws: completion ports have no
--   notion of readiness.  Use 'Network.Socket.recvBufWith', or one of
--   the other operations which give up, instead.
waitReadSocketSTM :: Socket -> IO (STM ())
waitReadSocketSTM s = fst <$> waitAndCancelReadSocketSTM s

-- | STM action to wait until the socket is ready for reading and STM
--   action to cancel the waiting.
--
--   __POSIX only.__  See 'waitReadSocketSTM'.
waitAndCancelReadSocketSTM :: Socket -> IO (STM (), IO ())
#if defined(mingw32_HOST_OS)
waitAndCancelReadSocketSTM _ =
    ioError $
        userError $
            "waitAndCancelReadSocketSTM: Windows completion ports do not "
                ++ "provide readiness notification; use recvBufWith or "
                ++ "Network.Socket.ByteString.recvWith instead"
#else
waitAndCancelReadSocketSTM s = withFdSocket s $ threadWaitReadSTM . Fd
#endif

-- | STM action to wait until the socket is ready for writing.
--
--   __POSIX only.__  On Windows this throws.  Completion ports give no
--   way to ask whether a send would block, and none is needed: issue
--   the send and let it complete asynchronously.
waitWriteSocketSTM :: Socket -> IO (STM ())
waitWriteSocketSTM s = fst <$> waitAndCancelWriteSocketSTM s

-- | STM action to wait until the socket is ready for writing and STM
--   action to cancel the waiting.
--
--   __POSIX only.__  See 'waitWriteSocketSTM'.
waitAndCancelWriteSocketSTM :: Socket -> IO (STM (), IO ())
#if defined(mingw32_HOST_OS)
waitAndCancelWriteSocketSTM _ =
    ioError $
        userError $
            "waitAndCancelWriteSocketSTM: Windows completion ports do not "
                ++ "provide readiness notification, and none is needed for "
                ++ "sending: issue the send instead"
#else
waitAndCancelWriteSocketSTM s = withFdSocket s $ threadWaitWriteSTM . Fd
#endif
