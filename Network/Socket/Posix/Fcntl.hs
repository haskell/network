{-# LANGUAGE CPP #-}

-- | The POSIX side of "Network.Socket.Fcntl".
module Network.Socket.Posix.Fcntl (
    setNonBlockIfNeeded,
    setCloseOnExecIfNeeded,
    getCloseOnExec,
    getNonBlock,
) where

import qualified System.Posix.Internals

import Network.Socket.Cbits
import Network.Socket.Imports
import Network.Socket.Types

setNonBlockIfNeeded :: CSocket -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD (fromIntegral fd) True

setCloseOnExecIfNeeded :: CSocket -> IO ()
#if defined(ghcjs_HOST_OS)
setCloseOnExecIfNeeded _ = return ()
#else
setCloseOnExecIfNeeded fd = System.Posix.Internals.setCloseOnExec fd
#endif

getCloseOnExec :: CSocket -> IO Bool
#if defined(ghcjs_HOST_OS)
getCloseOnExec _ = return False
#else
getCloseOnExec fd = do
    flags <- c_fcntl_get fd fGetFd
    let ret = flags .&. fdCloexec
    return (ret /= 0)
#endif

getNonBlock :: CSocket -> IO Bool
getNonBlock fd = do
    flags <- c_fcntl_get fd fGetFl
    let ret = flags .&. oNonBlock
    return (ret /= 0)

foreign import ccall unsafe "hsnet_fcntl_get"
  c_fcntl_get :: CSocket -> CInt -> IO CInt
