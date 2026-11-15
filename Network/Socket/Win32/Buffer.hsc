{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

##include "HsNetDef.h"
#  include "winsock2.h"
#  include "windows.h"
#  include "mswsock.h"
#  include "ntstatus.h"

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
import System.IO.Error (catchIOError)

import qualified GHC.Event.Windows as Mgr
import Network.Socket.Win32.CmsgHdr
import Network.Socket.Win32.Load
import Network.Socket.Win32.MsgHdr
import Network.Socket.Win32.WSABuf

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Name
import Network.Socket.Types
import Network.Socket.Flag

type DWORD   = Word32
type LPDWORD = Ptr DWORD

-- Windows error/status codes for WinIO completion callbacks.
-- Defined here so they are expanded by hsc2hs and available inside
-- ##if blocks that are deferred to GHC's CPP.
_ERROR_SUCCESS, _ERROR_IO_PENDING, _ERROR_HANDLE_EOF,
  _ERROR_BROKEN_PIPE, _ERROR_NO_MORE_ITEMS,
  _ERROR_OPERATION_ABORTED, _ERROR_IO_INCOMPLETE,
  _WSAECONNABORTED, _WSAECONNRESET, _WSAEDISCON,
  _WSAEMSGSIZE, _WSAESHUTDOWN, _STATUS_BUFFER_OVERFLOW :: Num a => a
_ERROR_SUCCESS           = #{const ERROR_SUCCESS}
_ERROR_IO_PENDING        = #{const ERROR_IO_PENDING}
_ERROR_HANDLE_EOF        = #{const ERROR_HANDLE_EOF}
_ERROR_BROKEN_PIPE       = #{const ERROR_BROKEN_PIPE}
_ERROR_NO_MORE_ITEMS     = #{const ERROR_NO_MORE_ITEMS}
_ERROR_OPERATION_ABORTED = #{const ERROR_OPERATION_ABORTED}
_ERROR_IO_INCOMPLETE     = #{const ERROR_IO_INCOMPLETE}
_WSAECONNABORTED         = #{const WSAECONNABORTED}
_WSAECONNRESET           = #{const WSAECONNRESET}
_WSAEDISCON              = #{const WSAEDISCON}
_WSAEMSGSIZE             = #{const WSAEMSGSIZE}
_WSAESHUTDOWN            = #{const WSAESHUTDOWN}
_STATUS_BUFFER_OVERFLOW  = #{const STATUS_BUFFER_OVERFLOW}

sendBufTo :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
sendBufTo s ptr nbytes sa =
  withSocketAddress sa $ \p_sa siz ->
    withFdSocket s $ \sock ->
      with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \wbuf ->
        fmap fromIntegral $ throwWinIOError "Network.Socket.sendBufTo" $
          Mgr.withOverlapped "sendBufTo" (wordPtrToPtr $ fromIntegral sock) 0
            (\lpOverlapped -> do
              ret <- c_WSASendTo sock wbuf 1 nullPtr 0 p_sa
                       (fromIntegral siz) (castPtr lpOverlapped) nullPtr
              if ret == 0
                then return Mgr.CbPending
                else c_WSAGetLastError >>= \err ->
                  return $ if err == _ERROR_IO_PENDING
                    then Mgr.CbPending
                    else Mgr.CbError (fromIntegral err))
            (\err bytes -> if err == _ERROR_SUCCESS
              then Mgr.ioSuccess $ fromIntegral bytes
              else Mgr.ioFailed err)

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf s ptr len = withFdSocket s $ \sock ->
  with (WSABuf ptr (fromIntegral len)) $ \wbuf ->
    fmap fromIntegral $ throwWinIOError "Network.Socket.sendBuf" $
      Mgr.withOverlapped "sendBuf" (wordPtrToPtr $ fromIntegral sock) 0
        (\lpOverlapped -> do
          ret <- c_WSASend sock wbuf 1 nullPtr 0 (castPtr lpOverlapped) nullPtr
          if ret == 0
            then return Mgr.CbPending
            else c_WSAGetLastError >>= \err ->
              return $ if err == _ERROR_IO_PENDING
                then Mgr.CbPending
                else Mgr.CbError (fromIntegral err))
        (\err bytes -> if err == _ERROR_SUCCESS
          then Mgr.ioSuccess $ fromIntegral bytes
          else Mgr.ioFailed err)

recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFrom s ptr nbytes =
    withNewSocketAddress $ \ptr_sa sz -> alloca $ \ptr_len ->
        withFdSocket s $ \sock -> do
            poke ptr_len (fromIntegral sz)
            len <- fmap fromIntegral $ throwWinIOError "Network.Socket.recvBufFrom" $
                Mgr.withOverlapped "recvBufFrom" (wordPtrToPtr $ fromIntegral sock) 0
                    (startCB sock ptr_sa ptr_len) completionCB
            sockaddr <- peekSocketAddress ptr_sa
                `catchIOError` \_ -> getPeerName s
            return (len, sockaddr)
  where
    startCB :: CSocket -> Ptr sa -> Ptr CInt -> Mgr.LPOVERLAPPED -> IO (Mgr.CbResult Int)
    startCB sock ptr_sa ptr_len lpOverlapped = do
        alloca $ \flags -> do
            poke flags 0
            with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \pWsaBuf -> do
                ret <- c_WSARecvFrom sock pWsaBuf 1 nullPtr flags ptr_sa ptr_len (castPtr lpOverlapped) nullPtr
                -- Check WSAGetLastError immediately: if the operation didn't
                -- complete synchronously (ret /= 0), we must distinguish
                -- ERROR_IO_PENDING (async completion forthcoming) from real
                -- errors (no IOCP notification will arrive, so CbPending
                -- would hang forever).
                err <- c_WSAGetLastError
                if ret == 0
                    -- An overlapped socket queues a completion packet even when
                    -- the call succeeds synchronously, so let the I/O manager
                    -- resolve it.  CbDone Nothing makes it read an OVERLAPPED
                    -- that may not be filled in yet, which surfaces as a
                    -- spurious EOF.
                    then return Mgr.CbPending
                    else if err == _ERROR_IO_PENDING
                        then return Mgr.CbPending
                        else return $ Mgr.CbError (fromIntegral err)

    completionCB err dwBytes
        | err == _ERROR_SUCCESS           = Mgr.ioSuccess $ fromIntegral dwBytes
        | err == _WSAECONNABORTED         = Mgr.ioSuccess 0
        | err == _WSAECONNRESET           = Mgr.ioFailed err
        | err == _WSAEDISCON              = Mgr.ioSuccess 0
        | err == _ERROR_HANDLE_EOF        = Mgr.ioSuccess 0
        | err == _ERROR_BROKEN_PIPE       = Mgr.ioSuccess 0
        | err == _ERROR_NO_MORE_ITEMS     = Mgr.ioSuccess 0
        | err == _ERROR_OPERATION_ABORTED = Mgr.ioSuccess 0
        | err == _ERROR_IO_INCOMPLETE     = Mgr.ioSuccess 0
        | otherwise                       = Mgr.ioFailed err

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf s ptr nbytes = withFdSocket s $ \sock ->
    fmap fromIntegral $ throwWinIOError "Network.Socket.recvBuf" $
        Mgr.withOverlapped "recvBuf" (wordPtrToPtr $ fromIntegral sock) 0 (startCB sock) completionCB
  where
    startCB :: CSocket -> Mgr.LPOVERLAPPED -> IO (Mgr.CbResult Int)
    startCB sock lpOverlapped = do
        alloca $ \flags -> do
            poke flags 0
            with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \pWsaBuf -> do
                ret <- c_WSARecv sock pWsaBuf 1 nullPtr flags (castPtr lpOverlapped) nullPtr
                -- Check WSAGetLastError immediately: if the operation didn't
                -- complete synchronously (ret /= 0), we must distinguish
                -- ERROR_IO_PENDING (async completion forthcoming) from real
                -- errors (no IOCP notification will arrive, so CbPending
                -- would hang forever).
                err <- c_WSAGetLastError
                if ret == 0
                    -- An overlapped socket queues a completion packet even when
                    -- the call succeeds synchronously, so let the I/O manager
                    -- resolve it.  CbDone Nothing makes it read an OVERLAPPED
                    -- that may not be filled in yet, which surfaces as a
                    -- spurious EOF.
                    then return Mgr.CbPending
                    else if err == _ERROR_IO_PENDING
                        then return Mgr.CbPending
                        else return $ Mgr.CbError (fromIntegral err)

    -- https://learn.microsoft.com/en-us/windows/win32/api/winsock2/nf-winsock2-wsarecv#return-value
    completionCB err dwBytes
        | err == _ERROR_SUCCESS           = Mgr.ioSuccess $ fromIntegral dwBytes
        | err == _WSAECONNABORTED         = Mgr.ioSuccess 0
        | err == _WSAECONNRESET           = Mgr.ioFailed err
        | err == _WSAEDISCON              = Mgr.ioSuccess 0
        | err == _ERROR_HANDLE_EOF        = Mgr.ioSuccess 0
        | err == _ERROR_BROKEN_PIPE       = Mgr.ioSuccess 0
        | err == _ERROR_NO_MORE_ITEMS     = Mgr.ioSuccess 0
        | err == _ERROR_OPERATION_ABORTED = Mgr.ioSuccess 0
        | err == _ERROR_IO_INCOMPLETE     = Mgr.ioSuccess 0
        | otherwise                       = Mgr.ioFailed err

recvBufNoWaitOr :: IO CInt -> Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWaitOr onError s ptr nbytes = withFdSocket s $ \fd -> do
    alloca $ \ptr_bytes -> do
      res <- c_ioctlsocket fd #{const FIONREAD} ptr_bytes
      avail <- peek ptr_bytes
      let failed = do
              err <- c_WSAGetLastError
              if err == #{const WSAEWOULDBLOCK}
                  || err == #{const WSAEINPROGRESS} then
                  return (-1)
                else
                  onError
      r <- if res == #{const NO_ERROR} && avail > 0 then do
               -- A pending error, such as an ICMP unreachable, counts as
               -- something to read, and recv takes it.
               n <- c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
               if n == -1 then failed else return n
           else if avail == 0 then
               -- Socket would block, could also mean socket is closed but
               -- can't distinguish
               return (-1)
           else failed
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
            len <-
                recvBufMsgWinIO fd msgHdrPtr
            sockaddr <- peekSocketAddress addrPtr `catchIOError` \_ -> getPeerName s
            hdr <- peek msgHdrPtr

            let flags' = MsgFlag $ fromIntegral (msgFlags hdr)
            -- If the control buffer was truncated (MSG_CTRUNC), the
            -- control data may be invalid and parsing could segfault.
            cmsgs <- if msgCtrl hdr == nullPtr || (msgFlags hdr .&. #{const MSG_CTRUNC}) /= 0
                     then return []
                     else parseCmsgs msgHdrPtr

            return (sockaddr, len, cmsgs, flags')

foreign import CALLCONV unsafe "ioctlsocket"
  c_ioctlsocket :: CSocket -> CLong -> Ptr CULong -> IO CInt
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
  fmap fromIntegral $ throwWinIOError "Network.Socket.sendBufMsg" $
    Mgr.withOverlapped "sendBufMsg" (wordPtrToPtr $ fromIntegral fd) 0
      (\lpOverlapped -> do
        ret <- sendMsg fd (castPtr msgHdrPtr) (fromIntegral cflags) nullPtr
                 (castPtr lpOverlapped) nullPtr
        if ret == 0
          then return Mgr.CbPending
          else c_WSAGetLastError >>= \err ->
            return $ if err == _ERROR_IO_PENDING
              then Mgr.CbPending
              else Mgr.CbError (fromIntegral err))
      (\err bytes -> if err == _ERROR_SUCCESS
        then Mgr.ioSuccess $ fromIntegral bytes
        else Mgr.ioFailed err)

recvBufMsgWinIO :: CSocket -> Ptr (MsgHdr sa) -> IO Int
recvBufMsgWinIO fd msgHdrPtr = do
    -- Perform async WSARecvMsg using withOverlapped
    -- (socket already associated in socket creation)
    recvMsg <- mkRecvMsgUnsafe <$> getWSARecvMsg fd
    fmap fromIntegral $ throwWinIOError "Network.Socket.recvMsg" $
      Mgr.withOverlapped "recvMsg" (wordPtrToPtr $ fromIntegral fd) 0 (startCB recvMsg) completionCB
  where
    startCB :: WSARecvMsgFn -> Mgr.LPOVERLAPPED -> IO (Mgr.CbResult Int)
    startCB recvMsg lpOverlapped = do
        ret <- recvMsg fd (castPtr msgHdrPtr) nullPtr (castPtr lpOverlapped) nullPtr
        clearCtrlOnTruncation msgHdrPtr ret
        -- Check WSAGetLastError immediately: if the operation didn't
        -- complete synchronously (ret /= 0), we must distinguish
        -- ERROR_IO_PENDING (async completion forthcoming) from real
        -- errors (no IOCP notification will arrive, so CbPending
        -- would hang forever).
        err <- c_WSAGetLastError
        if ret == 0
            -- An overlapped socket queues a completion packet even when
            -- the call succeeds synchronously, so let the I/O manager
            -- resolve it.  CbDone Nothing makes it read an OVERLAPPED
            -- that may not be filled in yet, which surfaces as a
            -- spurious EOF.
            then return Mgr.CbPending
            else if err == _ERROR_IO_PENDING
                then return Mgr.CbPending
                else return $ Mgr.CbError (fromIntegral err)

    completionCB err dwBytes
      | err == _ERROR_SUCCESS           = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == _WSAEMSGSIZE             = Mgr.ioSuccess $ fromIntegral dwBytes
      | err == _STATUS_BUFFER_OVERFLOW  = Mgr.ioSuccess $ fromIntegral dwBytes  -- truncated msg
      | err == _WSAECONNRESET           = Mgr.ioFailed err
      | err == _WSAECONNABORTED         = Mgr.ioSuccess 0
      | err == _WSAESHUTDOWN            = Mgr.ioSuccess 0
      | err == _WSAEDISCON              = Mgr.ioSuccess 0
      | err == _ERROR_HANDLE_EOF        = Mgr.ioSuccess 0
      | err == _ERROR_BROKEN_PIPE       = Mgr.ioSuccess 0
      | err == _ERROR_NO_MORE_ITEMS     = Mgr.ioSuccess 0
      | err == _ERROR_OPERATION_ABORTED = Mgr.ioSuccess 0
      | err == _ERROR_IO_INCOMPLETE     = Mgr.ioSuccess 0
      | otherwise                       = Mgr.ioFailed err

foreign import ccall unsafe "recv"
  c_recv :: CSocket -> Ptr CChar -> CSize -> CInt -> IO CInt
