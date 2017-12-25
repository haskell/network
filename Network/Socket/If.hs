{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.If (
#if defined(HAVE_IF_NAMETOINDEX)
    ifNameToIndex
#endif
  ) where

#if defined(HAVE_IF_NAMETOINDEX)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CUInt(..))

-- | Returns the index corresponding to the interface name.
ifNameToIndex :: String -> IO (Maybe Int)
ifNameToIndex ifname = do
  index <- withCString ifname c_if_nametoindex
  -- On failure zero is returned. We'll return Nothing.
  return $ if index == 0 then Nothing else Just $ fromIntegral index

foreign import CALLCONV safe "if_nametoindex"
   c_if_nametoindex :: CString -> IO CUInt
#endif
