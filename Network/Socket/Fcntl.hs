{-# LANGUAGE CPP #-}

module Network.Socket.Fcntl where

import Network.Socket.Types
import qualified System.Posix.Internals

#if !defined(mingw32_HOST_OS)
import Network.Socket.Cbits
import Network.Socket.Imports
#endif

-- | Set the nonblocking flag on Unix.
--   On Windows, nothing is done.
setNonBlockIfNeeded :: CSocket -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD (fromIntegral fd) True

-- TODO: remove fromIntegral for WinIO

-- | Set the close_on_exec flag on Unix.
--   On Windows, nothing is done.
--
--   Since 2.7.0.0.
setCloseOnExecIfNeeded :: CSocket -> IO ()
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
setCloseOnExecIfNeeded _ = return ()
#else
setCloseOnExecIfNeeded fd = System.Posix.Internals.setCloseOnExec fd
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "hsnet_fcntl_get"
  c_fcntl_get :: CSocket -> CInt -> IO CInt
#endif

-- | Get the close_on_exec flag.
--   On Windows, this function always returns 'False'.
--
--   Since 2.7.0.0.
getCloseOnExec :: CSocket -> IO Bool
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
getCloseOnExec _ = return False
#else
getCloseOnExec fd = do
    flags <- c_fcntl_get fd fGetFd
    let ret = flags .&. fdCloexec
    return (ret /= 0)
#endif

-- | Get the nonblocking flag.
--   On Windows, this function always returns 'True': every socket is put
--   in non-blocking mode, as on POSIX.
--
--   Since 2.7.0.0.
getNonBlock :: CSocket -> IO Bool
#if defined(mingw32_HOST_OS)
getNonBlock _ = return True
#else
getNonBlock fd = do
    flags <- c_fcntl_get fd fGetFl
    let ret = flags .&. oNonBlock
    return (ret /= 0)
#endif
