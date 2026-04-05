{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

module Network.Socket.Buffer (
    sendBufTo
  , sendBuf
  , recvBufFrom
  , recvBuf
  , recvBufNoWait
  , sendBufMsg
  , recvBufMsg
  ) where

import Network.Socket.Imports
import Network.Socket.Types
import Network.Socket.Flag

#if defined(mingw32_HOST_OS)
import Network.Socket.Win32.CmsgHdr
#else
import Network.Socket.Posix.CmsgHdr
#endif

import qualified Network.Socket.Buffer.Posix as Posix
#if defined(mingw32_HOST_OS)
import qualified Network.Socket.Buffer.WinIO as Win
#endif

sendBufTo :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
#if defined(mingw32_HOST_OS)
sendBufTo = eitherSocket Posix.sendBufTo Win.sendBufTo
#else
sendBufTo = Posix.sendBufTo
#endif

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
#if defined(mingw32_HOST_OS)
sendBuf = eitherSocket Posix.sendBuf Win.sendBuf
#else
sendBuf = Posix.sendBuf
#endif

recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
#if defined(mingw32_HOST_OS)
recvBufFrom = eitherSocket Posix.recvBufFrom Win.recvBufFrom
#else
recvBufFrom = Posix.recvBufFrom
#endif

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
#if defined(mingw32_HOST_OS)
recvBuf = eitherSocket Posix.recvBuf Win.recvBuf
#else
recvBuf = Posix.recvBuf
#endif

recvBufNoWait :: Socket -> Ptr Word8 -> Int -> IO Int
#if defined(mingw32_HOST_OS)
recvBufNoWait = eitherSocket Posix.recvBufNoWait Win.recvBufNoWait
#else
recvBufNoWait = Posix.recvBufNoWait
#endif

sendBufMsg :: SocketAddress sa
           => Socket -> sa -> [(Ptr Word8,Int)] -> [Cmsg] -> MsgFlag -> IO Int
#if defined(mingw32_HOST_OS)
sendBufMsg = eitherSocket Posix.sendBufMsg Win.sendBufMsg
#else
sendBufMsg = Posix.sendBufMsg
#endif

recvBufMsg :: SocketAddress sa
           => Socket -> [(Ptr Word8,Int)] -> Int -> MsgFlag
           -> IO (sa,Int,[Cmsg],MsgFlag)
#if defined(mingw32_HOST_OS)
recvBufMsg = eitherSocket Posix.recvBufMsg Win.recvBufMsg
#else
recvBufMsg = Posix.recvBufMsg
#endif
