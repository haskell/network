{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.Options.Posix (
    getSockOpt,
    setSockOpt
) where

##include "HsNetDef.h"

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types.Posix

getSockOpt :: forall a . Storable a => Socket -> CInt -> CInt -> IO a
getSockOpt s level opt = alloca $ \ptr -> do
    let sz = fromIntegral $ sizeOf (undefined :: a)
    withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
        throwSocketErrorIfMinus1Retry_ "Network.Socket.getSockOpt" $
            c_getsockopt fd level opt ptr ptr_sz
    peek ptr

setSockOpt :: Storable a => Socket -> CInt -> CInt -> a -> IO ()
setSockOpt s level opt v = with v $ \ptr -> void $ do
    let sz = fromIntegral $ sizeOf v
    withFdSocket s $ \fd ->
      throwSocketErrorIfMinus1_ "Network.Socket.setSockOpt" $
      c_setsockopt fd level opt ptr sz

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
