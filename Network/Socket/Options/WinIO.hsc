{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.Options.WinIO (
    getSockOpt,
    setSockOpt
) where

##include "HsNetDef.h"

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types.WinIO

getSockOpt :: forall a . Storable a => Socket -> CInt -> CInt -> IO a
getSockOpt s level opt = alloca $ \ptr -> do
    let sz = fromIntegral $ sizeOf (undefined :: a)
    withSOCKET s $ \sock -> with sz $ \ptr_sz -> do
        throwSocketErrorIfMinus1Retry_ "Network.Socket.getSockOpt" $
            c_getsockopt sock level opt ptr ptr_sz
    peek ptr

setSockOpt :: Storable a => Socket -> CInt -> CInt -> a -> IO ()
setSockOpt s level opt v = with v $ \ptr -> void $ do
    let sz = fromIntegral $ sizeOf v
    withSOCKET s $ \sock ->
      throwSocketErrorIfMinus1_ "Network.Socket.setSockOpt" $
      c_setsockopt sock level opt ptr sz

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: SOCKET -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: SOCKET -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
