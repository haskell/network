{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.If (
#if HAVE_DECL_IF_NAMETOINDEX
    ifNameToIndex
  , ifIndexToName
#endif
  ) where

#if HAVE_DECL_IF_NAMETOINDEX
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CUInt(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)

-- | Returns the index corresponding to the interface name.
--
--   Since 3.0.0.0.
ifNameToIndex :: String -> IO (Maybe Int)
ifNameToIndex ifname = do
  index <- withCString ifname c_if_nametoindex
  -- On failure zero is returned. We'll return Nothing.
  return $ if index == 0 then Nothing else Just $ fromIntegral index

-- | Returns the interface name corresponding to the index.
--
--   Since 3.0.0.0.
ifIndexToName :: Int -> IO (Maybe String)
ifIndexToName ifn = allocaBytes 16 $ \ptr -> do -- 16 == IFNAMSIZ
    r <- c_if_indextoname (fromIntegral ifn) ptr
    if (r == nullPtr ) then
        return Nothing
      else
        Just `fmap` peekCString ptr

foreign import CALLCONV safe "if_nametoindex"
   c_if_nametoindex :: CString -> IO CUInt

foreign import CALLCONV safe "if_indextoname"
   c_if_indextoname :: CUInt -> CString -> IO CString
#endif
