{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-- | The Windows parts of "Network.Socket.ByteString": gathering writes.
module Network.Socket.Win32.ByteString (
    sendManyOnce,
    sendManyToOnce,
) where

import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.ByteString.Internal (withBufSizs)
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types
import Network.Socket.Win32.Load
import Network.Socket.Win32.MsgHdr (MsgHdr (..))
import Network.Socket.Win32.WSABuf

type DWORD   = Word32
type LPDWORD = Ptr DWORD

-- | One gathering write, which may send only part of the data.
sendManyOnce :: Socket -> [ByteString] -> IO Int
sendManyOnce s cs =
    fmap fromIntegral . withWSABuffromBS cs $ \(wsabsPtr, wsabsLen) ->
        withFdSocket s $ \fd -> do
            let len = fromIntegral wsabsLen
            alloca $ \send_ptr -> do
                _ <- throwSocketErrorWaitWrite s "Network.Socket.ByteString.sendMany" $
                       c_wsasend fd wsabsPtr len send_ptr 0 nullPtr nullPtr
                peek send_ptr

-- | One gathering write to the address, which may send only part of the
--   data.
sendManyToOnce :: Socket -> [ByteString] -> SockAddr -> IO Int
sendManyToOnce s cs addr =
    fmap fromIntegral $ withSockAddr addr $ \addrPtr addrSize ->
        withWSABuffromBS cs $ \(wsabsPtr, wsabsLen) -> do
            let msgHdr = MsgHdr {
                    msgName      = addrPtr
                  , msgNameLen   = fromIntegral addrSize
                  , msgBuffer    = wsabsPtr
                  , msgBufferLen = fromIntegral wsabsLen
                  , msgCtrl      = nullPtr
                  , msgCtrlLen   = 0
                  , msgFlags     = 0
                  }
            withFdSocket s $ \fd -> do
                wsaSendMsg <- mkSendMsgUnsafe <$> getWSASendMsg fd
                with msgHdr $ \msgHdrPtr ->
                    alloca $ \send_ptr -> do
                        _ <- throwSocketErrorWaitWrite s "Network.Socket.ByteString.sendManyTo" $
                                wsaSendMsg fd (castPtr msgHdrPtr) 0 send_ptr nullPtr nullPtr
                        peek send_ptr

-- An array of WSABuf made from the parts, and its length.
withWSABuffromBS :: [ByteString] -> ((Ptr WSABuf, Int) -> IO a) -> IO a
withWSABuffromBS cs f = withBufSizs cs $ \bufsizs -> withWSABuf bufsizs f

foreign import CALLCONV unsafe "WSASend"
  c_wsasend :: CSocket -> Ptr WSABuf -> DWORD -> LPDWORD -> DWORD -> Ptr () -> Ptr () -> IO CInt
