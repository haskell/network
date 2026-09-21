{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | STM interfaces for sockets.
--
-- Two styles are offered here:
--
-- * Readiness: 'waitReadSocketSTM' and friends return an 'STM' action
--   that becomes available once the socket is ready.  This mirrors
--   @select@\/@epoll@ and is __POSIX only__: Windows completion ports
--   report that an operation has finished, not that one could be
--   started, so there is no readiness to wait for.  All four throw on
--   Windows.
--
-- * Completion: 'recvBufSTM' and 'recvBufFromSTM' start a receive and
--   return an 'STM' action that delivers its result.  These work on
--   every platform and are what you want if the code has to run on
--   Windows.  "Network.Socket.ByteString" has 'ByteString' versions.
module Network.Socket.STM (
    -- * Waiting for readiness (POSIX only)
    waitReadSocketSTM,
    waitAndCancelReadSocketSTM,
    waitWriteSocketSTM,
    waitAndCancelWriteSocketSTM,

    -- * Receiving through STM (all platforms)
    recvBufSTM,
    recvBufFromSTM,

    -- * Building block
    viaSTM,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Network.Socket.Buffer
import Network.Socket.Imports
import Network.Socket.Types
#if !defined(mingw32_HOST_OS)
import Control.Concurrent (threadWaitReadSTM, threadWaitWriteSTM)
import System.Posix.Types (Fd (..))
#endif

-- | STM action to wait until the socket is ready for reading.
--
--   __POSIX only.__  On Windows this throws: completion ports have no
--   notion of readiness.  Use 'recvBufFromSTM', or
--   @Network.Socket.ByteString.recvFromSTM@, instead.
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
                ++ "provide readiness notification; use recvBufFromSTM or "
                ++ "Network.Socket.ByteString.recvFromSTM instead"
#else
waitAndCancelReadSocketSTM s = withFdSocket s $ threadWaitReadSTM . Fd . fromIntegral
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
waitAndCancelWriteSocketSTM s = withFdSocket s $ threadWaitWriteSTM . Fd . fromIntegral
#endif

-- | Start receiving into the given buffer and return an 'STM' action
--   delivering the number of bytes received, together with an action
--   cancelling the receive.  If the receive fails, the 'STM' action
--   rethrows the exception.
--
--   Unlike 'waitReadSocketSTM' this works on Windows, because it waits
--   for a completion rather than for readiness.
--
--   Two consequences follow from that, and both matter when composing
--   with 'orElse':
--
--   * Cancelling can lose data.  The receive may already have taken a
--     datagram out of the kernel queue, and that datagram is then gone.
--     Only abandon the 'STM' action on paths where losing it is
--     acceptable, such as shutdown.
--
--   * Cancelling needs the receive to be interruptible.  It is on POSIX
--     and under WinIO, but not under the old Windows I/O manager, where
--     the receive blocks in a foreign call and the cancel action waits
--     for it.
--
--   * The buffer must stay alive until the 'STM' action completes or
--     the cancel action returns.
recvBufSTM :: Socket -> Ptr Word8 -> Int -> IO (STM Int, IO ())
recvBufSTM s ptr nbytes = viaSTM $ recvBuf s ptr nbytes

-- | 'recvBufSTM' for unconnected sockets, also returning the peer
--   address.  The same caveats apply.
recvBufFromSTM
    :: SocketAddress sa => Socket -> Ptr Word8 -> Int -> IO (STM (Int, sa), IO ())
recvBufFromSTM s ptr nbytes = viaSTM $ recvBufFrom s ptr nbytes

-- | Run a blocking socket operation in a separate thread and hand its
--   result over through STM, together with an action cancelling it.
--   This is what 'recvBufSTM' is built from; the same caveats about
--   cancelling and about buffer lifetime apply to anything built with
--   it.
viaSTM :: IO a -> IO (STM a, IO ())
viaSTM act = do
    var <- newTVarIO Nothing
    tid <- forkIO $ E.try act >>= atomically . writeTVar var . Just
    let wait =
            readTVar var >>= \case
                Nothing -> retry
                Just (Left e) -> throwSTM (e :: E.SomeException)
                Just (Right x) -> return x
    return (wait, killThread tid)
