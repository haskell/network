module Network.Socket.SockAddr (
      getPeerName
    , getSocketName
    , connect
    , bind
    , accept
    , sendBufTo
    , recvBufFrom
    ) where

import Foreign.Ptr
import qualified Network.Socket.Buffer as G
import qualified Network.Socket.Name as G
import qualified Network.Socket.Syscall as G
import Network.Socket.Types

-- | Getting peer's 'SockAddr'.
getPeerName :: Socket -> IO SockAddr
getPeerName = G.getPeerName

-- | Getting my 'SockAddr'.
getSocketName :: Socket -> IO SockAddr
getSocketName = G.getSocketName

connect :: Socket -> SockAddr -> IO ()
connect = G.connect

bind :: Socket -> SockAddr -> IO ()
bind = G.bind

accept :: Socket -> IO (Socket, SockAddr)
accept = G.accept

sendBufTo :: Socket -> Ptr a -> Int -> SockAddr -> IO Int
sendBufTo = G.sendBufTo

recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom = G.recvBufFrom
