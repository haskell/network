{-# LANGUAGE CPP #-}

#include "HsNet.h"

-- | The POSIX parts of "Network.Socket.ByteString": gathering writes.
module Network.Socket.Posix.ByteString (
    sendManyOnce,
    sendManyToOnce,
    c_writev,
) where

import Data.ByteString (ByteString)
import Foreign.Marshal.Utils (with)
import System.Posix.Types (CSsize (..))

import Network.Socket.ByteString.Internal (withBufSizs)
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Posix.IOVec
import Network.Socket.Posix.MsgHdr (MsgHdr (..))
import Network.Socket.Types

-- | One gathering write, which may send only part of the data.
sendManyOnce :: Socket -> [ByteString] -> IO Int
sendManyOnce s cs =
    fmap fromIntegral . withIOVecfromBS cs $ \(iovsPtr, iovsLen) ->
        withFdSocket s $ \fd -> do
            let len = fromIntegral $ min iovsLen (#const IOV_MAX)
            throwSocketErrorWaitWrite s "Network.Socket.ByteString.sendMany" $
                c_writev fd iovsPtr len

-- | One gathering write to the address, which may send only part of the
--   data.
sendManyToOnce :: Socket -> [ByteString] -> SockAddr -> IO Int
sendManyToOnce s cs addr =
    fmap fromIntegral $ withSockAddr addr $ \addrPtr addrSize ->
        withIOVecfromBS cs $ \(iovsPtr, iovsLen) -> do
            let msgHdr = MsgHdr {
                    msgName    = addrPtr
                  , msgNameLen = fromIntegral addrSize
                  , msgIov     = iovsPtr
                  , msgIovLen  = fromIntegral iovsLen
                  , msgCtrl    = nullPtr
                  , msgCtrlLen = 0
                  , msgFlags   = 0
                  }
            withFdSocket s $ \fd ->
                with msgHdr $ \msgHdrPtr ->
                    throwSocketErrorWaitWrite s "Network.Socket.ByteString.sendManyTo" $
                        c_sendmsg fd msgHdrPtr 0

-- An array of IOVec made from the parts, and its length.
withIOVecfromBS :: [ByteString] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVecfromBS cs f = withBufSizs cs $ \bufsizs -> withIOVec bufsizs f

foreign import ccall unsafe "writev"
  c_writev :: CSocket -> Ptr IOVec -> CInt -> IO CSsize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CSocket -> Ptr (MsgHdr SockAddr) -> CInt -> IO CSsize
