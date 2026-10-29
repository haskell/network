-- | This module provides extensible APIs for socket addresses.
module Network.Socket.Address (
    -- * Socket Address
    SocketAddress (..),
    getPeerName,
    getSocketName,

    -- * Socket operations
    connect,
    bind,
    accept,
    acceptWith,

    -- * Sending and receiving ByteString
    sendTo,
    sendAllTo,
    recvFrom,
    recvWith,
    recvFromWith,

    -- * Sending and receiving data from a buffer
    sendBufTo,
    recvBufFrom,

    -- * Advanced IO
    sendBufMsg,
    recvBufMsg,
) where

import Network.Socket.Buffer
import Network.Socket.ByteString.IO
import Network.Socket.Name
import Network.Socket.Syscall
import Network.Socket.Types
import Network.Socket.Wait
