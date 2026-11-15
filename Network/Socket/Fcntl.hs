{-# LANGUAGE CPP #-}

module Network.Socket.Fcntl (
    setNonBlockIfNeeded,
    setCloseOnExecIfNeeded,
    getCloseOnExec,
    getNonBlock,
) where

import Network.Socket.Types
#if defined(mingw32_HOST_OS)
import qualified Network.Socket.Win32.Fcntl as P
#else
import qualified Network.Socket.Posix.Fcntl as P
#endif

-- | Set the nonblocking flag on Unix.
--   On Windows, nothing is done.
setNonBlockIfNeeded :: CSocket -> IO ()
setNonBlockIfNeeded = P.setNonBlockIfNeeded

-- | Set the close_on_exec flag on Unix.
--   On Windows, nothing is done.
--
--   Since 2.7.0.0.
setCloseOnExecIfNeeded :: CSocket -> IO ()
setCloseOnExecIfNeeded = P.setCloseOnExecIfNeeded

-- | Get the close_on_exec flag.
--   On Windows, this function always returns 'False'.
--
--   Since 2.7.0.0.
getCloseOnExec :: CSocket -> IO Bool
getCloseOnExec = P.getCloseOnExec

-- | Get the nonblocking flag.
--   On Windows, this function always returns 'True': every socket is put
--   in non-blocking mode, as on POSIX.
--
--   Since 2.7.0.0.
getNonBlock :: CSocket -> IO Bool
getNonBlock = P.getNonBlock
