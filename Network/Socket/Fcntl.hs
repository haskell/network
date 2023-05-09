{-# LANGUAGE CPP #-}

module Network.Socket.Fcntl where

import qualified System.Posix.Internals

#if !defined(mingw32_HOST_OS)
import Network.Socket.Cbits
#else
# if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
# endif
#endif
import Network.Socket.Imports

-- | Set the nonblocking flag on Unix.
--   On Windows, nothing is done.
setNonBlockIfNeeded :: CInt -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD fd True

-- | Set the close_on_exec flag on Unix.
--   On Windows, nothing is done.
--
--   Since 2.7.0.0.
setCloseOnExecIfNeeded :: CInt -> IO ()
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
setCloseOnExecIfNeeded _ = return ()
#else
setCloseOnExecIfNeeded fd = System.Posix.Internals.setCloseOnExec fd
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "fcntl"
  c_fcntl_read  :: CInt -> CInt -> CInt -> IO CInt
#endif

-- | Get the close_on_exec flag.
--   On Windows, this function always returns 'False'.
--
--   Since 2.7.0.0.
getCloseOnExec :: CInt -> IO Bool
#if defined(mingw32_HOST_OS) || defined(ghcjs_HOST_OS)
getCloseOnExec _ = return False
#else
getCloseOnExec fd = do
    flags <- c_fcntl_read fd fGetFd 0
    let ret = flags .&. fdCloexec
    return (ret /= 0)
#endif

-- | Get the nonblocking flag.
--   On Windows, this function always returns 'False' when using MIO but
--   returns `True` when using WinIO.  Technically on Windows whether the
--   the socket blocks or not is not determined by the socket itself but
--   by the operations used on the socket.  Becuase we will always use
--   overlapping I/O when WinIO is enabled we return `True` here.
--
--   Since 2.7.0.0.
getNonBlock :: CInt -> IO Bool
#if defined(mingw32_HOST_OS)
# if defined(__IO_MANAGER_WINIO__)
getNonBlock _ = return False <!> return True
# else
getNonBlock _ = return False 
# endif
#else
getNonBlock fd = do
    flags <- c_fcntl_read fd fGetFl 0
    let ret = flags .&. oNonBlock
    return (ret /= 0)
#endif
