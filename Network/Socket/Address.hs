module Network.Socket.Address (
    -- * Socket Address
      SocketAddress(..)
    , getPeerName
    , getSocketName
    -- * Socket operations
    , connect
    , bind
    , accept
    -- ** Sending and receiving data
    , sendBufTo
    , recvBufFrom
    ) where

import Network.Socket.Buffer
import Network.Socket.Name
import Network.Socket.Syscall
import Network.Socket.Types
