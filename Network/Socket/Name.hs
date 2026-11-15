{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.Name (
    getPeerName
  , getSocketName
  , peekReceivedAddress
  , socketPort
  , socketPortSafe
  ) where

import Foreign.Marshal.Utils (with)
import System.IO.Error (catchIOError)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

-- | Getting peer's socket address.
getPeerName :: SocketAddress sa => Socket -> IO sa
getPeerName s =
 withNewSocketAddress $ \ptr sz ->
   with (fromIntegral sz) $ \int_star -> withFdSocket s $ \fd -> do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.getPeerName" $
       c_getpeername fd ptr int_star
     len <- peek int_star
     peekSocketAddressLen ptr (fromIntegral len)

-- | Getting my socket address.
getSocketName :: SocketAddress sa => Socket -> IO sa
getSocketName s =
 withNewSocketAddress $ \ptr sz ->
   with (fromIntegral sz) $ \int_star -> withFdSocket s $ \fd -> do
     throwSocketErrorIfMinus1Retry_ "Network.Socket.getSocketName" $
       c_getsockname fd ptr int_star
     len <- peek int_star
     peekSocketAddressLen ptr (fromIntegral len)

-- | Reading the source address that a receive has returned with its
--   length.  With none, as on a stream socket, it is the peer of a
--   connected socket, or else an unnamed one.
peekReceivedAddress :: SocketAddress sa => Socket -> Ptr sa -> Int -> IO sa
peekReceivedAddress s ptr 0 =
    getPeerName s `catchIOError` \_ -> peekSocketAddressLen ptr 0
peekReceivedAddress s ptr len =
    peekSocketAddressLen ptr len `catchIOError` \_ -> getPeerName s

foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CSocket -> Ptr sa -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CSocket -> Ptr sa -> Ptr CInt -> IO CInt

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

-- | Getting the port of socket.
--   `IOError` is thrown if a port is not available.
socketPort :: Socket            -- Connected & Bound Socket
           -> IO PortNumber     -- Port Number of Socket
socketPort s = do
    sa <- getSocketName s
    case sa of
      SockAddrInet port _      -> return port
      SockAddrInet6 port _ _ _ -> return port
      _                        -> ioError $ userError "Network.Socket.socketPort: AF_UNIX not supported."

-- ---------------------------------------------------------------------------
-- socketPortSafe
-- | Getting the port of socket.
socketPortSafe :: Socket                -- Connected & Bound Socket
               -> IO (Maybe PortNumber) -- Port Number of Socket
socketPortSafe s = do
    sa <- getSocketName s
    return $ case sa of
      SockAddrInet port _      -> Just port
      SockAddrInet6 port _ _ _ -> Just port
      _                        -> Nothing
