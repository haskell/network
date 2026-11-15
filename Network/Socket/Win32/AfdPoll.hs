-- | Waiting for a socket to become ready on Windows, through
--   @IOCTL_AFD_POLL@ on the AFD driver beneath Winsock, as libuv,
--   wepoll and mio do.  It completes on the I/O manager's port like any
--   overlapped operation on the socket.
module Network.Socket.Win32.AfdPoll (
    afdRead,
    afdWrite,
    afdWait,
    afdWaitSTM,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (STM, atomically, newEmptyTMVarIO, putTMVar, readTMVar, throwSTM)
import qualified Control.Exception as E
import Foreign.C.Types (CInt (..), CUIntPtr (..), CULong (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, wordPtrToPtr)
import qualified GHC.Event.Windows as Mgr

import Network.Socket.Types

-- | RECEIVE, DISCONNECT, ACCEPT, ABORT and LOCAL_CLOSE: whatever a read
--   would not block on, as POSIX readiness reports it.
afdRead :: CULong
afdRead = 0x0001 + 0x0008 + 0x0080 + 0x0010 + 0x0020

-- | SEND, CONNECT_FAIL, ABORT and LOCAL_CLOSE.
afdWrite :: CULong
afdWrite = 0x0004 + 0x0100 + 0x0010 + 0x0020

-- | Blocking until one of the events is reported on the socket.  An
--   asynchronous exception ends the wait: 'Mgr.withOverlapped' cancels
--   the poll.
afdWait :: String -> CULong -> Socket -> IO ()
afdWait name events s = withFdSocket s $ \fd -> do
    size <- c_afd_poll_info_size
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
  where
    errorIoPending = 997

-- | 'afdWait' as an STM action, and an action cancelling it.  The wait
--   runs in a thread of its own, which the cancel kills.
afdWaitSTM :: String -> CULong -> Socket -> IO (STM (), IO ())
afdWaitSTM name events s = do
    var <- newEmptyTMVarIO
    tid <- forkIO $ do
        r <- E.try $ afdWait name events s
        atomically $ putTMVar var (r :: Either E.IOException ())
    return (readTMVar var >>= either throwSTM return, killThread tid)

foreign import ccall unsafe "hsnet_afd_poll_info_size"
    c_afd_poll_info_size :: IO CInt
foreign import ccall unsafe "hsnet_afd_poll"
    c_afd_poll :: CSocket -> CULong -> Ptr () -> Ptr () -> IO CInt
