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
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar, throwSTM)
import qualified Control.Exception as E
import Foreign.C.Types (CInt (..), CULong (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, wordPtrToPtr)
import qualified GHC.Event.Windows as Mgr
import GHC.IO.SubSystem ((<!>))
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
        <!> afdWait "waitAndCancelReadSocketSTM" afdRead _s
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
        <!> afdWait "waitAndCancelWriteSocketSTM" afdWrite _s
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

#if defined(mingw32_HOST_OS) && __IO_MANAGER_WINIO__ >= 2
-- | Waiting through @IOCTL_AFD_POLL@, which completes on the I/O
--   manager's port like any operation on the socket.  Cancelling kills
--   the waiting thread, and 'Mgr.withOverlapped' cancels the poll.
afdWait :: String -> CULong -> Socket -> IO (STM (), IO ())
afdWait name events s = do
    var <- newEmptyTMVarIO
    size <- c_afd_poll_info_size
    tid <- forkIO $ do
        r <- E.try $ withFdSocket s $ \fd ->
            allocaBytes (fromIntegral size) $ \info -> fmap (const ()) $
                Mgr.withException name $
                    Mgr.withOverlapped name (wordPtrToPtr $ fromIntegral fd) 0
                        (\ov -> do
                            rc <- c_afd_poll fd events info (castPtr ov)
                            return $
                                if rc == 0 || rc == errorIoPending
                                    then Mgr.CbPending
                                    else Mgr.CbError (fromIntegral rc))
                        (\err _ ->
                            if err == 0 then Mgr.ioSuccess 0 else Mgr.ioFailed err)
        atomically $ putTMVar var (r :: Either E.IOException ())
    return (readTMVar var >>= either throwSTM return, killThread tid)
  where
    errorIoPending = 997

-- RECEIVE, DISCONNECT, ACCEPT, ABORT and LOCAL_CLOSE: whatever a read
-- would not block on, as POSIX readiness reports it.
afdRead :: CULong
afdRead = 0x0001 + 0x0008 + 0x0080 + 0x0010 + 0x0020

-- SEND, CONNECT_FAIL, ABORT and LOCAL_CLOSE.
afdWrite :: CULong
afdWrite = 0x0004 + 0x0100 + 0x0010 + 0x0020

foreign import ccall unsafe "hsnet_afd_poll_info_size"
    c_afd_poll_info_size :: IO CInt
foreign import ccall unsafe "hsnet_afd_poll"
    c_afd_poll :: CSocket -> CULong -> Ptr () -> Ptr () -> IO CInt
#endif
