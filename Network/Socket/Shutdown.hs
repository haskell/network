{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNetDef.h"

module Network.Socket.Shutdown (
    ShutdownCmd(..)
  , shutdown
  , gracefulClose
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, yield)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Foreign.Marshal.Alloc (mallocBytes, free)
import System.Timeout

import Network.Socket.Buffer
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types
import Network.Socket.Wait

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

-- Waiting for the peer's FIN, draining what arrives before it.  The
-- receive is given up on rather than interrupted: an asynchronous
-- exception does not reach a thread blocked in one on Windows.
recvEOF :: Socket -> Int -> Ptr Word8 -> IO ()
recvEOF s tmout0 buf
    | not rtsSupportsBoundThreads = recvEOFtimeout s tmout0 buf
    | otherwise = do
        -- millisecond to microsecond
        late <- registerDelay $ tmout0 * 1000
        loop late 0
  where
    loop late n0 = do
        r <- E.try $ recvBufWith s buf bufSize $ check =<< readTVar late
        case r of
            Left (_ :: E.IOException) -> return ()
            -- The deadline.
            Right (Left ()) -> return ()
            Right (Right n1) -> when (n1 > 0) $ do
                let n = n0 + n1
                when (n < drainLimit) $ loop late n

-- registerDelay needs -threaded, and so does the give-up on Windows --
-- where this cannot cut a receive short either, but nothing else works
-- there without it.
recvEOFtimeout :: Socket -> Int -> Ptr Word8 -> IO ()
-- millisecond to microsecond
recvEOFtimeout s tmout0 buf = void $ timeout (tmout0 * 1000) $ recvBuf s buf bufSize

-- Don't use 4092 here. The GHC runtime takes the global lock
-- if the length is over 3276 bytes in 32bit or 3272 bytes in 64bit.
bufSize :: Int
bufSize = 1024

-- Data left unread makes close send RST rather than FIN, so what has
-- arrived is drained -- but only this far: nothing is going to read it.
drainLimit :: Int
drainLimit = 128 * 1024
