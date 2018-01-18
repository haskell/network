{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

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

-- | Getting peer's 'SockAddr'.
getPeerName   :: Socket -> IO SockAddr
getPeerName Socket{..} =
 withNewSockAddr socketFamily $ \ptr sz ->
   with (fromIntegral sz) $ \int_star -> do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.getPeerName" $
       c_getpeername socketFd' ptr int_star
     _sz <- peek int_star
     peekSockAddr ptr

-- | Getting my 'SockAddr'.
getSocketName :: Socket -> IO SockAddr
getSocketName Socket{..} =
 withNewSockAddr socketFamily $ \ptr sz ->
   with (fromIntegral sz) $ \int_star -> do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.getSocketName" $
       c_getsockname socketFd' ptr int_star
     peekSockAddr ptr

foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

-- | Getting the port of socket.
socketPort :: Socket            -- Connected & Bound Socket
           -> IO PortNumber     -- Port Number of Socket
socketPort sock@(Socket _ AF_INET _ _) = do
    (SockAddrInet port _) <- getSocketName sock
    return port
socketPort sock@(Socket _ AF_INET6 _ _) = do
    (SockAddrInet6 port _ _ _) <- getSocketName sock
    return port
socketPort Socket{..} =
    ioError $ userError $
      "Network.Socket.socketPort: address family '" ++ show socketFamily ++
      "' not supported."
