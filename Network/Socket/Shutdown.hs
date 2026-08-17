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
-- The deadline is enforced by a watchdog thread calling 'abortRecv',
-- rather than by 'System.Timeout.timeout': with the MIO manager on
-- Windows, 'recvBuf' blocks in a foreign 'recv' call, which the
-- asynchronous exception thrown by 'timeout' cannot interrupt.  The
-- watchdog itself only ever blocks in 'threadDelay', which is always
-- interruptible, so 'killThread' reliably reaps it once EOF is
-- reached.
recvEOFloop :: Socket -> Int -> Ptr Word8 -> IO ()
recvEOFloop s tmout0 buf = E.bracket watchdog killThread $ \_ -> loop 0
  where
    watchdog = forkIO $ do
        threadDelay (tmout0 * 1000)
        abortRecv s
    loop n0 = do
        ex <- E.tryIOError $ recvBuf s buf bufSize
        case ex of
            Left _   -> return ()
            Right n1 -> when (n1 > 0) $ do
                let n = n0 + n1
                when (n < drainLimit) $ loop n

-- Aborting the drain loop's 'recvBuf' while leaving the descriptor
-- valid, so that, unlike 'close', nothing here can race with
-- descriptor reuse.
--
-- 'shutdown' makes every recv issued from now on fail (POSIX: EOF;
-- Windows: WSAESHUTDOWN) and on POSIX it also wakes a recv that is
-- already blocked in the kernel.  On Windows it does not, so a
-- blocked recv is aborted with CancelIoEx: sockets are created with
-- WSA_FLAG_OVERLAPPED, hence even a "blocking" recv is an overlapped
-- operation internally, waited on inside ws2_32, and cancellation
-- makes it fail with WSA_OPERATION_ABORTED.  Shutting down first
-- closes the race with a recv that has not yet entered the kernel:
-- in every interleaving the recv returns EOF, fails, or is
-- cancelled, and each of these ends the drain loop.
abortRecv :: Socket -> IO ()
abortRecv s = do
    void $ E.tryIOError $ shutdown s ShutdownBoth
#if defined(mingw32_HOST_OS)
    void $ withFdSocket s $ \fd -> c_CancelIoEx fd nullPtr
#endif

#if defined(mingw32_HOST_OS)
-- BOOL CancelIoEx(HANDLE hFile, LPOVERLAPPED lpOverlapped)
-- A SOCKET is a kernel HANDLE.  A NULL lpOverlapped cancels all
-- pending I/O on the handle, whichever thread issued it.
foreign import CALLCONV unsafe "CancelIoEx"
  c_CancelIoEx :: CSocket -> Ptr () -> IO CInt
#endif
