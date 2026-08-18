module Network.Socket.Posix.HostName (
    getHostName,
)
where

#include "HsNet.h"

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Network.Socket.Info (HostName)

foreign import ccall unsafe "gethostname"
    gethostname :: CString -> CSize -> IO CInt

-- | Get name of current host
--
--   Since: 3.3.0.0
getHostName :: IO HostName
getHostName = allocaArray0 size $ \cstr -> do
    throwErrnoIfMinus1_ "getHostName" $ gethostname cstr (fromIntegral size)
    peekCString cstr
  where
    size = 256
