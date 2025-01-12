{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.If (
    ifNameToIndex
  , ifIndexToName
  ) where

import Foreign.Marshal.Alloc (allocaBytes)

import Network.Socket.Imports
#if !defined(HAVE_IF_NAMETOINDEX) || !defined(HAVE_IF_INDEXTONAME)
import Network.Socket.Internal (unsupported)
#endif

-- | Returns the index corresponding to the interface name.
--
--   Since 2.7.0.0.
ifNameToIndex :: String -> IO (Maybe Int)
#ifdef HAVE_IF_NAMETOINDEX
ifNameToIndex ifname = do
  index <- withCString ifname c_if_nametoindex
  -- On failure zero is returned. We'll return Nothing.
  return $ if index == 0 then Nothing else Just $ fromIntegral index
#else
ifNameToIndex _ = unsupported "ifNameToIndex"
{-# WARNING ifNameToIndex "operation will throw 'IOError' \"unsupported operation\"" #-}
#endif

-- | Returns the interface name corresponding to the index.
--
--   Since 2.7.0.0.
ifIndexToName :: Int -> IO (Maybe String)
#ifdef HAVE_IF_INDEXTONAME
ifIndexToName ifn = allocaBytes 16 $ \ptr -> do -- 16 == IFNAMSIZ
    r <- c_if_indextoname (fromIntegral ifn) ptr
    if r == nullPtr then
        return Nothing
      else
        Just <$> peekCString ptr
#else
ifIndexToName _ = unsupported "ifIndexToName"
{-# WARNING ifIndexToName "operation will throw 'IOError' \"unsupported operation\"" #-}
#endif

#ifdef HAVE_IF_NAMETOINDEX
foreign import CALLCONV safe "if_nametoindex"
   c_if_nametoindex :: CString -> IO CUInt
#endif

#ifdef HAVE_IF_INDEXTONAME
foreign import CALLCONV safe "if_indextoname"
   c_if_indextoname :: CUInt -> CString -> IO CString
#endif
