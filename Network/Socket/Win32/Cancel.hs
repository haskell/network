{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

#include "HsNetDef.h"

-- | Giving up on an operation on a socket, on Windows.
--
--   There is no readiness to wait for here -- completion ports report
--   that an operation has finished, not that one could be started -- so
--   the operation is run on a thread of its own and ended with
--   @CancelIoEx@.  Nothing else would end it: an asynchronous exception
--   does not reach a thread blocked in a socket call on Windows.
--
--   __These need the threaded runtime.__  The operation blocks in a
--   foreign call, which without @-threaded@ stops every other thread,
--   including the one which would cancel it.
module Network.Socket.Win32.Cancel (
    windowsViaSTM,
    windowsCancelPendingIo,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Foreign.Ptr (nullPtr)
import System.IO (hPutStrLn, stderr)

import Network.Socket.Imports
import Network.Socket.Types

-- | Running a blocking operation on the socket in a thread of its own
--   and handing its result over through STM, together with an action
--   cancelling it.  If the operation fails, the 'STM' action rethrows
--   the exception.
--
--   Cancelling waits for the operation to end, so a buffer it reads
--   into has to stay alive until either the 'STM' action completes or
--   the cancel returns.
windowsViaSTM :: Socket -> IO a -> IO (STM a, IO ())
windowsViaSTM s act = do
    var <- newTVarIO Nothing
    -- Masked so that the var is written whatever happens: a thread
    -- which died between the call returning and the write would leave
    -- the waiter retrying and the cancel below looping for ever.  The
    -- call itself is a foreign one, which no exception reaches anyway.
    tid <- forkIO $ E.mask_ $ E.try act >>= atomically . writeTVar var . Just
    let wait =
            readTVar var >>= \case
                Nothing -> retry
                Just (Left e) -> throwSTM (e :: E.SomeException)
                Just (Right x) -> return x
        ended = isJust <$> readTVar var
    return (wait, cancel s ended tid)
  where
    -- An asynchronous exception does not reach a thread blocked in a
    -- socket call here, so the call itself is cancelled.  CancelIoEx
    -- reaches an operation which is already in the kernel, and a call
    -- which has not got there yet is caught by trying again.  The
    -- socket is never shut down to end the call: the caller is entitled
    -- to keep using it, and a shutdown would take that away.
    --
    -- Uninterruptible because this must not be abandoned: the operation
    -- is still writing into the caller's buffer until it ends, and
    -- nothing but the end of it stops the loop.
    cancel sock ended _tid = E.uninterruptibleMask_ $ loop sayAfter
      where
        loop n = do
            windowsCancelPendingIo sock
            over <- endedWithin 1000
            unless over $ do
                when (n == 1) say
                loop (n - 1)
        -- Wait for the call to end rather than sleep through it:
        -- CancelIoEx usually works at once, and a sleep here costs a
        -- whole timer tick, which Windows measures in tens of ms.
        endedWithin us = do
            late <- registerDelay us
            atomically $
                (True <$ (check =<< ended))
                    `orElse` (False <$ (check =<< readTVar late))
    -- Rounds before saying something and carrying on.  A round which
    -- cancels nothing costs a timer tick, which Windows measures in tens
    -- of milliseconds, so this is seconds rather than the hundredth of a
    -- second it reads as.  Two rounds is the most ever measured.
    sayAfter :: Int
    sayAfter = 100
    -- Said from a thread of its own, and its failure ignored.  This is
    -- the path which has to finish before the caller's buffer can go,
    -- and writing to stderr can block on the handle's lock or on a pipe
    -- nobody is draining -- for ever, inside the mask above.
    say = void $ forkIO $ void (E.try warn :: IO (Either E.SomeException ()))
    warn =
        hPutStrLn stderr $
            "network: a socket operation given up on is not cancelling; "
                ++ "still trying.  Please report this."

-- | Cancelling whatever operation is pending on the socket, while
--   leaving the socket itself usable: unlike 'Network.Socket.shutdown'
--   this does not shut it down.  An operation which has not reached the
--   kernel yet is not cancelled, so the caller has to be ready to try
--   again.
windowsCancelPendingIo :: Socket -> IO ()
windowsCancelPendingIo s = void $ withFdSocket s $ \fd -> c_CancelIoEx fd nullPtr

-- BOOL CancelIoEx(HANDLE hFile, LPOVERLAPPED lpOverlapped)
-- A SOCKET is a kernel HANDLE.  A NULL lpOverlapped cancels all
-- pending I/O on the handle, whichever thread issued it.
foreign import CALLCONV unsafe "CancelIoEx"
  c_CancelIoEx :: CInt -> Ptr () -> IO CInt
