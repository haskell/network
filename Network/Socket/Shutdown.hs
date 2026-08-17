{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNetDef.h"

module Network.Socket.Shutdown (
    ShutdownCmd(..)
  , shutdown
  , gracefulClose
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay, yield)
import qualified Control.Exception as E
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified System.IO.Error as E

import Network.Socket.Buffer
import Network.Socket.Imports
import Network.Socket.Internal
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
shutdown s stype = shutdown' `annotateIOException` show s
  where
    shutdown' =
      void $ withFdSocket s $ \fd ->
        throwSocketErrorIfMinus1Retry_ "Network.Socket.shutdown" $
          c_shutdown fd $ sdownCmdToInt stype

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CSocket -> CInt -> IO CInt

-- | Closing a socket gracefully.
--   This sends TCP FIN and check if TCP FIN is received from the peer.
--   The second argument is time out to receive TCP FIN in millisecond.
--   In both normal cases and error cases, socket is deallocated finally.
--
--   Since: 3.1.1.0
gracefulClose :: Socket -> Int -> IO ()
gracefulClose s tmout0 =
    (sendRecvFIN `E.finally` close s) `annotateIOException` show s
  where
    sendRecvFIN = do
        -- Sending TCP FIN.
        ex <- E.tryIOError $ shutdown s ShutdownSend
        case ex of
          -- Don't catch asynchronous exceptions
          Left _ -> return ()
          Right () -> do
              -- Giving CPU time to other threads hoping that
              -- FIN arrives meanwhile.
              yield
              -- Waiting TCP FIN.
              E.bracket (mallocBytes bufSize) free (recvEOFloop s tmout0)

-- Don't use 4092 here. The GHC runtime takes the global lock
-- if the length is over 3276 bytes in 32bit or 3272 bytes in 64bit.
bufSize :: Int
bufSize = 1024

-- Maximum number of bytes to drain while waiting for the peer's FIN.
drainLimit :: Int
drainLimit = 128 * 1024

-- Draining the receive queue until EOF, bounded by 'drainLimit' bytes
-- and by the millisecond deadline in the second argument.
--
-- The deadline is enforced by a watchdog thread that shuts the socket
-- down, rather than by 'System.Timeout.timeout': with the MIO manager
-- on Windows, 'recvBuf' blocks in a foreign 'recv' call, which the
-- asynchronous exception thrown by 'timeout' cannot interrupt.
-- 'shutdown' aborts a blocked 'recv' while leaving the descriptor
-- valid, so unlike 'close' it does not race with a 'recvBuf' that has
-- not yet entered the kernel: whichever side wins, 'recvBuf' returns
-- EOF or fails, both of which end the loop.  The watchdog itself only
-- ever blocks in 'threadDelay', which is always interruptible, so
-- 'killThread' reliably reaps it once EOF is reached.
recvEOFloop :: Socket -> Int -> Ptr Word8 -> IO ()
recvEOFloop s tmout0 buf = E.bracket watchdog killThread $ \_ -> loop 0
  where
    watchdog = forkIO $ do
        threadDelay (tmout0 * 1000)
        void $ E.tryIOError $ shutdown s ShutdownBoth
    loop n0 = do
        ex <- E.tryIOError $ recvBuf s buf bufSize
        case ex of
            Left _   -> return ()
            Right n1 -> when (n1 > 0) $ do
                let n = n0 + n1
                when (n < drainLimit) $ loop n
