-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.Safe
-- Copyright   :  Echo Nolan 2016
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A drop in replacement for "Network.Socket" that sacrifices some performance
-- for correctness. Specifically, this module's functions check that a socket
-- hasn't been closed before attempting to read or write from it. With the
-- "Network.Socket" API, reading or writing to a socket after closing it causes
-- undefined behavior. In this module it always throws an exception. N.b. this
-- serializes all use of a given socket via an MVar.
--
-- See "Network.Socket" for API documentation.
-----------------------------------------------------------------------------

-- We reexport things with attached warnings, but we don't want those warnings
-- here.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Network.Socket.Safe
  (
    module Network.Socket
  , send
  , sendTo
  , recv
  , recvFrom
  , recvLen
  , sendBuf
  , recvBuf
  , sendBufTo
  , recvBufFrom
  ) where

import Network.Socket hiding (send, sendTo, recv, recvFrom, recvLen, sendBuf,
                              recvBuf, sendBufTo, recvBufFrom)
import qualified Network.Socket as Unsafe

import Network.Socket.Internal

import Data.Word (Word8)
import Foreign.Ptr (Ptr)

{-# WARNING send "Use send defined in \"Network.Socket.ByteString.Safe\"" #-}
send :: Socket -> String -> IO Int
send = wrapCheckStatus2 Unsafe.send "Network.Socket.Safe.send"

{-# WARNING sendTo "Use sendTo defined in \"Network.Socket.ByteString.Safe\"" #-}
sendTo :: Socket -> String -> SockAddr -> IO Int
sendTo = wrapCheckStatus3 Unsafe.sendTo "Network.Socket.Safe.sendTo"

{-# WARNING recv "Use recv defined in \"Network.Socket.ByteString.Safe\"" #-}
recv :: Socket -> Int -> IO String
recv = wrapCheckStatus2 Unsafe.recv "Network.Socket.Safe.recv"

{-# WARNING recvFrom "Use recvFrom defined in \"Network.Socket.ByteString.Safe\"" #-}
recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom = wrapCheckStatus2 Unsafe.recvFrom "Network.Socket.Safe.recvFrom"

{-# WARNING recvLen "Use recvLen defined in \"Network.Socket.ByteString.Safe\"" #-}
recvLen :: Socket -> Int -> IO (String, Int)
recvLen = wrapCheckStatus2 Unsafe.recvLen "Network.Socket.Safe.recvLen"

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf = wrapCheckStatus3 Unsafe.sendBuf "Network.Socket.Safe.sendBuf"

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf = wrapCheckStatus3 Unsafe.recvBuf "Network.Socket.Safe.recvBuf"

sendBufTo :: Socket -> Ptr a -> Int -> SockAddr -> IO Int
sendBufTo = wrapCheckStatus4 Unsafe.sendBufTo "Network.Socket.Safe.sendBufTo"

recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom = wrapCheckStatus3 Unsafe.recvBufFrom "Network.Socket.Safe.recvBufFrom"
