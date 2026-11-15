-- | The POSIX side of "Network.Socket.Internal": errors come from errno,
--   and the IO manager waits for readiness.
module Network.Socket.Posix.Internal (
    throwSocketError,
    throwSocketErrorCode,
    throwSocketErrorIfMinus1_,
    throwSocketErrorIfMinus1Retry,
    throwSocketErrorIfMinus1RetryMayBlock,
    throwSocketErrorIfMinus1RetryMayBlockBut,
    waitReadSocket,
    waitWriteSocket,
    withSocketsDo,
) where

import Foreign.C.Error (
    Errno (..),
    errnoToIOError,
    throwErrno,
    throwErrnoIfMinus1Retry,
    throwErrnoIfMinus1RetryMayBlock,
    throwErrnoIfMinus1_,
 )
import GHC.Conc (threadWaitRead, threadWaitWrite)

import Network.Socket.Imports
import Network.Socket.Types

throwSocketError :: String -> IO a
throwSocketError = throwErrno

throwSocketErrorCode :: String -> CInt -> IO a
throwSocketErrorCode loc errno =
    ioError (errnoToIOError loc (Errno errno) Nothing Nothing)

throwSocketErrorIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_
{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

throwSocketErrorIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry
{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

throwSocketErrorIfMinus1RetryMayBlock
    :: (Eq a, Num a) => String -> IO b -> IO a -> IO a
throwSocketErrorIfMinus1RetryMayBlock name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block
{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}

throwSocketErrorIfMinus1RetryMayBlockBut
    :: (Eq a, Num a) => (CInt -> Bool) -> String -> IO b -> IO a -> IO a
throwSocketErrorIfMinus1RetryMayBlockBut _exempt =
    throwSocketErrorIfMinus1RetryMayBlock

waitReadSocket, waitWriteSocket :: Socket -> CSocket -> IO ()
waitReadSocket _ fd = threadWaitRead $ fromIntegral fd
waitWriteSocket _ fd = threadWaitWrite $ fromIntegral fd

withSocketsDo :: IO a -> IO a
withSocketsDo x = x
{-# INLINE withSocketsDo #-}
