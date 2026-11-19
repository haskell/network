{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

##include "HsNetDef.h"
#  include "winsock2.h"
#  include "windows.h"
#  include "mswsock.h"

module Network.Socket.Win32.Buffer (
    sendBufTo
  , sendBuf
  , recvBufFrom
  , recvBuf
  , recvBufNoWaitOr
  , sendBufMsg
  , recvBufMsg
  ) where

import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)

import Network.Socket.Win32.CmsgHdr
import Network.Socket.Win32.Load
import Network.Socket.Win32.MsgHdr
import Network.Socket.Win32.WSABuf

import Network.Socket.Imports
import Network.Socket.Name
import Network.Socket.Types
import Network.Socket.Win32.Internal (withOverlappedSocket)
import Network.Socket.Flag

type DWORD   = Word32
type LPDWORD = Ptr DWORD

-- | What a failed receive returns instead: a datagram too large for the
--   buffer is truncated to its size as on POSIX, and the connection ending
--   is the end of the data.
recvResult :: Int -> CInt -> Maybe Int
recvResult size rc
    | rc == #{const WSAEMSGSIZE} = Just size
    | rc `elem` eofs = Just 0
    | otherwise = Nothing
  where
    eofs =
        [ #{const WSAECONNABORTED}, #{const WSAESHUTDOWN}, #{const WSAEDISCON}
        , #{const ERROR_HANDLE_EOF}, #{const ERROR_BROKEN_PIPE}
        , #{const ERROR_NO_MORE_ITEMS}, #{const ERROR_OPERATION_ABORTED}
        , #{const ERROR_IO_INCOMPLETE} ]

-- | A failed send is always raised.
sendResult :: CInt -> Maybe Int
sendResult _ = Nothing

sendBufTo :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
sendBufTo s ptr nbytes sa =
  withSocketAddress sa $ \p_sa siz ->
    withFdSocket s $ \sock ->
      with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \wbuf ->
        withOverlappedSocket "Network.Socket.sendBufTo" sock
          (\ov -> c_WSASendTo sock wbuf 1 nullPtr 0 p_sa (fromIntegral siz)
                     (castPtr ov) nullPtr)
          sendResult

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf s ptr len = withFdSocket s $ \sock ->
  with (WSABuf ptr (fromIntegral len)) $ \wbuf ->
    withOverlappedSocket "Network.Socket.sendBuf" sock
      (\ov -> c_WSASend sock wbuf 1 nullPtr 0 (castPtr ov) nullPtr)
      sendResult

recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFrom s ptr nbytes =
    withNewSocketAddress $ \ptr_sa sz -> alloca $ \ptr_len ->
        withFdSocket s $ \sock -> do
            poke ptr_len (fromIntegral sz)
            len <- with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \wbuf ->
              alloca $ \flags ->
                withOverlappedSocket "Network.Socket.recvBufFrom" sock
                  (\ov -> do
                      poke flags 0
                      c_WSARecvFrom sock wbuf 1 nullPtr flags ptr_sa ptr_len
                          (castPtr ov) nullPtr)
                  (recvResult nbytes)
            sa_len <- peek ptr_len
            sockaddr <- peekReceivedAddress s ptr_sa (fromIntegral sa_len)
            return (len, sockaddr)

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf s ptr nbytes = withFdSocket s $ \sock ->
    with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \wbuf ->
      alloca $ \flags ->
        withOverlappedSocket "Network.Socket.recvBuf" sock
          (\ov -> do
              poke flags 0
              c_WSARecv sock wbuf 1 nullPtr flags (castPtr ov) nullPtr)
          (recvResult nbytes)

recvBufNoWaitOr :: IO CInt -> Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWaitOr onError s ptr nbytes = withFdSocket s $ \fd -> do
    -- The socket is non-blocking, as on POSIX, so recv tells the end of
    -- the data (0) from nothing to read yet (WSAEWOULDBLOCK).
    n <- c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    r <- if n /= -1 then return n else do
        err <- c_WSAGetLastError
        if err == #{const WSAEWOULDBLOCK} || err == #{const WSAEINPROGRESS}
            then return (-1)
            else onError
    return $ fromIntegral r

sendBufMsg :: SocketAddress sa
           => Socket            -- ^ Socket
           -> sa                -- ^ Destination address
           -> [(Ptr Word8,Int)] -- ^ Data to be sent
           -> [Cmsg]            -- ^ Control messages
           -> MsgFlag           -- ^ Message flags
           -> IO Int            -- ^ The length actually sent
sendBufMsg s sa bufsizs cmsgs flags = do
  sz <- withSocketAddress sa $ \addrPtr addrSize ->
    withWSABuf bufsizs $ \(wsaBPtr, wsaBLen) -> do
      withCmsgs cmsgs $ \ctrlPtr ctrlLen -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
              , msgBuffer    = wsaBPtr
              , msgBufferLen = fromIntegral wsaBLen
              , msgCtrl    = castPtr ctrlPtr
              , msgCtrlLen = fromIntegral ctrlLen
              , msgFlags   = 0
              }
            cflags = fromMsgFlag flags
        withFdSocket s $ \fd ->
          with msgHdr $ \msgHdrPtr ->
            sendBufMsgWinIO fd msgHdrPtr cflags
  return $ fromIntegral sz

recvBufMsg :: SocketAddress sa
           => Socket            -- ^ Socket
           -> [(Ptr Word8,Int)] -- ^ A list of (buffer, buffer-length) pairs.
                                --   If the total length is not large enough,
                                --   'MSG_TRUNC' is returned
           -> Int               -- ^ The buffer size for control messages.
                                --   If the length is not large enough,
                                --   'MSG_CTRUNC' is returned
           -> MsgFlag           -- ^ Message flags
           -> IO (sa,Int,[Cmsg],MsgFlag) -- ^ Source address, total bytes received, control messages and message flags
recvBufMsg s bufsizs clen flags = do
  withNewSocketAddress $ \addrPtr addrSize ->
    allocaBytes clen $ \ctrlPtr ->
      withWSABuf bufsizs $ \(wsaBPtr, wsaBLen) -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
              , msgBuffer    = wsaBPtr
              , msgBufferLen = fromIntegral wsaBLen
              , msgCtrl    = if clen == 0 then nullPtr else castPtr ctrlPtr
              , msgCtrlLen = fromIntegral clen
              , msgFlags   = fromIntegral $ fromMsgFlag flags
              }
            _cflags = fromMsgFlag flags
        withFdSocket s $ \fd -> do
          with msgHdr $ \msgHdrPtr -> do
            len <- recvBufMsgWinIO fd msgHdrPtr (sum $ map snd bufsizs)
            hdr <- peek msgHdrPtr
            sockaddr <- peekReceivedAddress s addrPtr (fromIntegral $ msgNameLen hdr)

            let flags' = MsgFlag $ fromIntegral (msgFlags hdr)
            -- If the control buffer was truncated (MSG_CTRUNC), the
            -- control data may be invalid and parsing could segfault.
            cmsgs <- if msgCtrl hdr == nullPtr || (msgFlags hdr .&. #{const MSG_CTRUNC}) /= 0
                     then return []
                     else parseCmsgs msgHdrPtr

            return (sockaddr, len, cmsgs, flags')

foreign import CALLCONV unsafe "WSAGetLastError"
  c_WSAGetLastError :: IO CInt
foreign import CALLCONV unsafe "WSASend"
  c_WSASend :: CSocket -> Ptr WSABuf -> DWORD -> LPDWORD -> DWORD -> Ptr () -> Ptr () -> IO CInt
foreign import CALLCONV unsafe "WSASendTo"
  c_WSASendTo :: CSocket -> Ptr WSABuf -> DWORD -> LPDWORD -> DWORD -> Ptr sa -> CInt -> Ptr () -> Ptr () -> IO CInt
foreign import CALLCONV unsafe "WSARecv"
  c_WSARecv :: CSocket -> Ptr WSABuf -> DWORD -> LPDWORD -> LPDWORD -> Ptr () -> Ptr () -> IO CInt
foreign import CALLCONV unsafe "WSARecvFrom"
  c_WSARecvFrom :: CSocket -> Ptr WSABuf -> DWORD -> LPDWORD -> LPDWORD -> Ptr sa -> Ptr CInt -> Ptr () -> Ptr () -> IO CInt

-- Winsock can leave the control buffer pointing at garbage once the
-- message was truncated, so clear it.  The C wrapper around WSARecvMsg
-- used to do this.
clearCtrlOnTruncation :: Ptr (MsgHdr sa) -> CInt -> IO ()
clearCtrlOnTruncation msgHdrPtr ret = when (ret == -1) $ do
    err <- c_WSAGetLastError
    when (err == #{const WSAEMSGSIZE}) $ do
        (#poke WSAMSG, Control.len) msgHdrPtr (0 :: Word32)
        (#poke WSAMSG, Control.buf) msgHdrPtr (nullPtr :: Ptr Word8)

sendBufMsgWinIO :: CSocket -> Ptr (MsgHdr sa) -> CInt -> IO CInt
sendBufMsgWinIO fd msgHdrPtr cflags = do
  sendMsg <- mkSendMsgUnsafe <$> getWSASendMsg fd
  fmap fromIntegral $ withOverlappedSocket "Network.Socket.sendBufMsg" fd
    (\ov -> sendMsg fd (castPtr msgHdrPtr) (fromIntegral cflags) nullPtr
               (castPtr ov) nullPtr)
    sendResult

recvBufMsgWinIO :: CSocket -> Ptr (MsgHdr sa) -> Int -> IO Int
recvBufMsgWinIO fd msgHdrPtr size = do
    recvMsg <- mkRecvMsgUnsafe <$> getWSARecvMsg fd
    withOverlappedSocket "Network.Socket.recvMsg" fd
      (\ov -> do
          ret <- recvMsg fd (castPtr msgHdrPtr) nullPtr (castPtr ov) nullPtr
          clearCtrlOnTruncation msgHdrPtr ret
          return ret)
      (recvResult size)

foreign import ccall unsafe "recv"
  c_recv :: CSocket -> Ptr CChar -> CSize -> CInt -> IO CInt
