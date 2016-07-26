-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.ByteString.Safe
-- Copyright   :  Echo Nolan 2016
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A drop in replacement for "Network.Socket.ByteString" that sacrifices some
-- performance for correctness. See "Network.Socket.Safe" for what exactly that
-- means. See "Network.Socket.ByteString" for API documentation.
-----------------------------------------------------------------------------

module Network.Socket.ByteString.Safe
  (
    send
  , sendAll
  , sendTo
  , sendAllTo
  , sendMany
  , sendManyTo
  , recv
  , recvFrom
  ) where

import qualified Network.Socket.ByteString as Unsafe
import Network.Socket.Internal
import Network.Socket.Types

import Data.ByteString (ByteString)

send :: Socket -> ByteString -> IO Int
send = wrapCheckStatus2 Unsafe.send "Network.Socket.ByteString.Safe.send"

sendAll :: Socket -> ByteString -> IO ()
sendAll = wrapCheckStatus2 Unsafe.sendAll "Network.Socket.ByteString.Safe.sendAll"

sendTo :: Socket -> ByteString -> SockAddr -> IO Int
sendTo = wrapCheckStatus3 Unsafe.sendTo "Network.Socket.ByteString.Safe.sendTo"

sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()
sendAllTo = wrapCheckStatus3 Unsafe.sendAllTo "Network.Socket.ByteString.Safe.sendAllTo"

sendMany :: Socket -> [ByteString] -> IO ()
sendMany = wrapCheckStatus2 Unsafe.sendMany "Network.Socket.ByteString.Safe.sendMany"

sendManyTo :: Socket -> [ByteString] -> SockAddr -> IO ()
sendManyTo = wrapCheckStatus3 Unsafe.sendManyTo "Network.Socket.ByteString.Safe.sendManyTo"

recv :: Socket -> Int -> IO ByteString
recv = wrapCheckStatus2 Unsafe.recv "Network.Socket.ByteString.Safe.recv"

recvFrom :: Socket -> Int -> IO (ByteString,SockAddr)
recvFrom = wrapCheckStatus2 Unsafe.recvFrom "Network.Socket.ByteString.Safe.recvFrom"
