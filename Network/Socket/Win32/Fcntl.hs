-- | The Windows side of "Network.Socket.Fcntl": there is no fcntl.
module Network.Socket.Win32.Fcntl (
    setNonBlockIfNeeded,
    setCloseOnExecIfNeeded,
    getCloseOnExec,
    getNonBlock,
) where

import qualified System.Posix.Internals

import Network.Socket.Types

setNonBlockIfNeeded :: CSocket -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD (fromIntegral fd) True

setCloseOnExecIfNeeded :: CSocket -> IO ()
setCloseOnExecIfNeeded _ = return ()

getCloseOnExec :: CSocket -> IO Bool
getCloseOnExec _ = return False

-- Every socket is non-blocking, as on POSIX.
getNonBlock :: CSocket -> IO Bool
getNonBlock _ = return True
