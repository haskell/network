-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.ByteString.Lazy.Safe
-- Copyright   :  Echo Nolan 2016
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A drop in replacement for "Network.Socket.ByteString.Lazy" that sacrifices
-- some performance for correctness. See "Network.Socket.Safe" for what exactly
-- that means. See "Network.Socket.ByteString.Lazy" for API documentation.
-----------------------------------------------------------------------------

module Network.Socket.ByteString.Lazy.Safe
  (
    send
  , sendAll
  , getContents
  , recv
  ) where

import qualified Network.Socket.ByteString.Lazy as Unsafe
import Network.Socket.Internal
import Network.Socket.Types

import Prelude hiding (getContents)
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)

send :: Socket -> ByteString -> IO Int64
send = wrapCheckStatus2 Unsafe.send "Network.Socket.ByteString.Lazy.Safe.send"

sendAll :: Socket -> ByteString -> IO ()
sendAll = wrapCheckStatus2 Unsafe.sendAll "Network.Socket.ByteString.Lazy.Safe.sendAll"

getContents :: Socket -> IO ByteString
getContents = wrapCheckStatus Unsafe.getContents "Network.Socket.ByteString.Lazy.Safe.getContents"

recv :: Socket -> Int64 -> IO ByteString
recv = wrapCheckStatus2 Unsafe.recv "Network.Socket.ByteString.Lazy.Safe.recv"
