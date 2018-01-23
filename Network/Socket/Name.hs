{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.Name (
    getPeerName
  , getSocketName
  , socketPort
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Network.Socket.Internal
import Network.Socket.Types

-- | Getting peer's socket address.
getPeerName :: SocketAddress sa => Socket -> IO sa
getPeerName s =
 withNewSocketAddress $ \ptr sz ->
   with (fromIntegral sz) $ \int_star -> do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.getPeerName" $
       c_getpeername (fdSocket s) ptr int_star
     _sz <- peek int_star
     peekSocketAddress ptr

-- | Getting my socket address.
getSocketName :: SocketAddress sa => Socket -> IO sa
getSocketName s =
 withNewSocketAddress $ \ptr sz ->
   with (fromIntegral sz) $ \int_star -> do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.getSocketName" $
       c_getsockname (fdSocket s) ptr int_star
     peekSocketAddress ptr

foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CInt -> Ptr sa -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CInt -> Ptr sa -> Ptr CInt -> IO CInt

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

-- | Getting the port of socket.
socketPort :: Socket            -- Connected & Bound Socket
           -> IO PortNumber     -- Port Number of Socket
socketPort s = do
    sa <- getSocketName s
    case sa of
      SockAddrInet port _      -> return port
      SockAddrInet6 port _ _ _ -> return port
      _                        -> ioError $ userError $ "Network.Socket.socketPort: AF_UNIX not supported."

