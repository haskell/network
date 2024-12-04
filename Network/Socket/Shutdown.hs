{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNetDef.h"

module Network.Socket.Shutdown (
    ShutdownCmd(..)
  , shutdown
  , gracefulClose
  ) where

import Control.Concurrent (yield)
import qualified Control.Exception as E
import Foreign.Marshal.Alloc (mallocBytes, free)
import System.Timeout

#if !defined(mingw32_HOST_OS)
import Control.Concurrent.STM
import qualified GHC.Event as Ev
#endif

import Network.Socket.Buffer
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.STM
import Network.Socket.Types

data ShutdownCmd = ShutdownReceive
                 | ShutdownSend
                 | ShutdownBoth

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

-- | Shut down one or both halves of the connection, depending on the
-- second argument to the function.  If the second argument is
-- 'ShutdownReceive', further receives are disallowed.  If it is
-- 'ShutdownSend', further sends are disallowed.  If it is
-- 'ShutdownBoth', further sends and receives are disallowed.
shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown s stype = void $ withFdSocket s $ \fd ->
  throwSocketErrorIfMinus1Retry_ "Network.Socket.shutdown" $
    c_shutdown fd $ sdownCmdToInt stype

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

-- | Closing a socket gracefully.
--   This sends TCP FIN and check if TCP FIN is received from the peer.
--   The second argument is time out to receive TCP FIN in millisecond.
--   In both normal cases and error cases, socket is deallocated finally.
--
--   Since: 3.1.1.0
gracefulClose :: Socket -> Int -> IO ()
gracefulClose s tmout0 = sendRecvFIN `E.finally` close s
  where
    sendRecvFIN = do
        -- Sending TCP FIN.
        ex <- E.try $ shutdown s ShutdownSend
        case ex of
          -- Don't catch asynchronous exceptions
          Left (_ :: E.IOException) -> return ()
          Right () -> do
              -- Giving CPU time to other threads hoping that
              -- FIN arrives meanwhile.
              yield
              -- Waiting TCP FIN.
              E.bracket (mallocBytes bufSize) free (recvEOF s tmout0)

recvEOF :: Socket -> Int -> Ptr Word8 -> IO ()
#if !defined(mingw32_HOST_OS)
recvEOF s tmout0 buf = do
    mevmgr <- Ev.getSystemEventManager
    case mevmgr of
      Nothing -> recvEOFtimeout s tmout0 buf
      Just _ -> recvEOFevent s tmout0 buf
#else
recvEOF = recvEOFtimeout
#endif

-- Don't use 4092 here. The GHC runtime takes the global lock
-- if the length is over 3276 bytes in 32bit or 3272 bytes in 64bit.
bufSize :: Int
bufSize = 1024

recvEOFtimeout :: Socket -> Int -> Ptr Word8 -> IO ()
recvEOFtimeout s tmout0 buf = void $ timeout tmout0 $ recvBuf s buf bufSize

#if !defined(mingw32_HOST_OS)
data Wait = MoreData | TimeoutTripped

recvEOFevent :: Socket -> Int -> Ptr Word8 -> IO ()
recvEOFevent s tmout0 buf = do
    tmmgr <- Ev.getSystemTimerManager
    tvar <- newTVarIO False
    E.bracket (setupTimeout tmmgr tvar) (cancelTimeout tmmgr) $ \_ -> do
        E.bracket (setupRead s) cancelRead $ \(rxWait,_) -> do
            let toWait = readTVar tvar >>= check
                wait = atomically ((toWait >> return TimeoutTripped)
                               <|> (rxWait >> return MoreData))
            waitRes <- wait
            case waitRes of
              TimeoutTripped -> return ()
              -- We don't check the (positive) length.
              -- In normal case, it's 0. That is, only FIN is received.
              -- In error cases, data is available. But there is no
              -- application which can read it. So, let's stop receiving
              -- to prevent attacks.
              MoreData       -> void $ recvBufNoWait s buf bufSize
  where
    -- millisecond to microsecond
    tmout = tmout0 * 1000
    setupTimeout tmmgr tvar =
        Ev.registerTimeout tmmgr tmout $ atomically $ writeTVar tvar True
    cancelTimeout = Ev.unregisterTimeout
    setupRead = waitAndCancelReadSocketSTM
    cancelRead (_,cancel) = cancel
#endif
