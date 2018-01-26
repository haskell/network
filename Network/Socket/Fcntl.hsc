#include "HsNet.h"

module Network.Socket.Fcntl where

import qualified System.Posix.Internals

import Foreign.C.Types (CInt(..))

#if defined(mingw32_HOST_OS)
#else
import Data.Bits ((.&.))
#endif

-- | Set the nonblocking flag on Unix.
--   On Windows, nothing is done.
setNonBlockIfNeeded :: CInt -> IO ()
setNonBlockIfNeeded fd =
    System.Posix.Internals.setNonBlockingFD fd True

-- | Set the close_on_exec flag on Unix.
--   On Windows, nothing is done.
--
--   Since 3.0.0.0.
setCloseOnExecIfNeeded :: CInt -> IO ()
#if defined(mingw32_HOST_OS)
setCloseOnExecIfNeeded _ = return ()
#else
setCloseOnExecIfNeeded fd = System.Posix.Internals.setCloseOnExec fd
#endif

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "fcntl"
  c_fcntl_read  :: CInt -> CInt -> CInt -> IO CInt
#endif

-- | Get the nonblocking flag.
--   On Windows, this function always returns 'False'.
--
--   Since 3.0.0.0.
getCloseOnExec :: CInt -> IO Bool
#if defined(mingw32_HOST_OS)
getCloseOnExec _ = return False
#else
getCloseOnExec fd = do
    flags <- c_fcntl_read fd (#const F_GETFD) 0
    let ret = flags .&. (#const FD_CLOEXEC)
    return (ret /= 0)
#endif

-- | Get the close_on_exec flag.
--   On Windows, this function always returns 'False'.
--
--   Since 3.0.0.0.
getNonBlock :: CInt -> IO Bool
#if defined(mingw32_HOST_OS)
getNonBlock _ = return False
#else
getNonBlock fd = do
    flags <- c_fcntl_read fd (#const F_GETFL) 0
    let ret = flags .&. (#const O_NONBLOCK)
    return (ret /= 0)
#endif
