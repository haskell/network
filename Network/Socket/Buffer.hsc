{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

##include "HsNetDef.h"
#if defined(mingw32_HOST_OS)
#  include "winsock2.h"
#  include "windows.h"
#  include "mswsock.h"
#  include "ntstatus.h"
#endif

module Network.Socket.Buffer (
    sendBufTo
  , sendBuf
  , recvBufFrom
  , recvBuf
  , recvBufNoWait
  , recvBufNoWaitOr
  , sendBufMsg
  , recvBufMsg
  ) where

#if !defined(mingw32_HOST_OS)
import Foreign.C.Error (getErrno, eAGAIN, eWOULDBLOCK)
#endif
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import System.IO.Error (mkIOError, ioeSetErrorString, catchIOError)

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as Mgr
import Network.Socket.Win32.CmsgHdr
import Network.Socket.Win32.Load
import Network.Socket.Win32.MsgHdr
import Network.Socket.Win32.WSABuf
#else
import Network.Socket.Posix.CmsgHdr
import Network.Socket.Posix.MsgHdr
import Network.Socket.Posix.IOVec
#endif

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Name
import Network.Socket.Types
import Network.Socket.Flag

#if defined(mingw32_HOST_OS)
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
#endif

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
sendBufTo :: SocketAddress sa =>
             Socket -- (possibly) bound/connected Socket
          -> Ptr a
          -> Int         -- Data to send
          -> sa
          -> IO Int      -- Number of Bytes sent
#if defined(mingw32_HOST_OS)
sendBufTo = sendBufToWinIO
#else
sendBufTo = sendBufToPosix
#endif

#if !defined(mingw32_HOST_OS)
sendBufToPosix :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
sendBufToPosix s ptr nbytes sa =
  withSocketAddress sa $ \p_sa siz -> fromIntegral <$> do
    withFdSocket s $ \fd -> do
        let sz = fromIntegral siz
            n = fromIntegral nbytes
            flags = 0
        throwSocketErrorWaitWrite s "Network.Socket.sendBufTo" $
          c_sendto fd ptr n flags p_sa sz
#endif

#if defined(mingw32_HOST_OS)
sendBufToWinIO :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
sendBufToWinIO s ptr nbytes sa =
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
#endif

-- | Send data to the socket. The socket must be connected to a remote
-- socket. Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
sendBuf :: Socket    -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> IO Int     -- Number of Bytes sent
#if defined(mingw32_HOST_OS)
sendBuf = sendBufWinIO
#else
sendBuf s str len =
    fromIntegral <$> withFdSocket s (\fd -> do
        let flags = 0
            clen = fromIntegral len
        throwSocketErrorWaitWrite s "Network.Socket.sendBuf" $
          c_send fd str clen flags)
#endif

#if defined(mingw32_HOST_OS)
sendBufWinIO :: Socket -> Ptr Word8 -> Int -> IO Int
sendBufWinIO s ptr len = withFdSocket s $ \sock ->
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
#endif

-- | Receive data from the socket, writing it into buffer instead of
-- creating a new string.  The socket need not be in a connected
-- state. Returns @(nbytes, address)@ where @nbytes@ is the number of
-- bytes received and @address@ is a 'SockAddr' representing the
-- address of the sending socket.
--
-- If the first return value is zero, it means EOF.
--
-- For 'Stream' sockets, the second return value would be invalid.
recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFrom s ptr nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBufFrom")
#if defined(mingw32_HOST_OS)
    | otherwise   = recvBufFromWinIO s ptr nbytes
#else
    | otherwise   = recvBufFromImpl s ptr nbytes
#endif

#if !defined(mingw32_HOST_OS)
recvBufFromImpl :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFromImpl s ptr nbytes =
    withNewSocketAddress $ \ptr_sa sz -> alloca $ \ptr_len ->
        withFdSocket s $ \fd -> do
            poke ptr_len (fromIntegral sz)
            let cnbytes = fromIntegral nbytes
                flags = 0
            len <- throwSocketErrorWaitRead s "Network.Socket.recvBufFrom" $
                     c_recvfrom fd ptr cnbytes flags ptr_sa ptr_len
            sockaddr <- peekSocketAddress ptr_sa
                `catchIOError` \_ -> getPeerName s
            return (fromIntegral len, sockaddr)
#endif

#if defined(mingw32_HOST_OS)
recvBufFromWinIO :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFromWinIO s ptr nbytes =
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
#endif

-- | Receive data from the socket.  The socket must be in a connected
-- state. This function may return fewer bytes than specified.  If the
-- message is longer than the specified length, it may be discarded
-- depending on the type of socket.  This function may block until a
-- message arrives.
--
-- Considering hardware and network realities, the maximum number of
-- bytes to receive should be a small power of 2, e.g., 4096.
--
-- The return value is the length of received data. Zero means
-- EOF. Historical note: Version 2.8.x.y or earlier,
-- an EOF error was thrown. This was changed in version 3.0.
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf s ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBuf")
#if defined(mingw32_HOST_OS)
 | otherwise   = recvBufWinIO s ptr nbytes
#else
 | otherwise   = do
    len <- withFdSocket s $ \fd ->
        throwSocketErrorWaitRead s "Network.Socket.recvBuf" $
             c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    return $ fromIntegral len
#endif

#if defined(mingw32_HOST_OS)
recvBufWinIO :: Socket -> Ptr Word8 -> Int -> IO Int
recvBufWinIO s ptr nbytes = withFdSocket s $ \sock ->
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

#endif /* mingw32_HOST_OS */

-- | Receive data from the socket. This function returns immediately
--   even if data is not available. In other words, IO manager is NOT
--   involved. The length of data is returned if received.
--   -1 is returned in the case of EAGAIN or EWOULDBLOCK.
--   -2 is returned in other error cases.
recvBufNoWait :: Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWait = recvBufNoWaitOr $ return (-2)

-- | 'recvBufNoWait' which runs the action on an error, in place of
--   answering -2.  The error is gone from the socket once read, so this
--   is where to raise it.
recvBufNoWaitOr :: IO CInt -> Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWaitOr onError s ptr nbytes = withFdSocket s $ \fd -> do
#if defined(mingw32_HOST_OS)
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

#else
    r <- c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    if r >= 0 then
        return $ fromIntegral r
      else do
        err <- getErrno
        if err == eAGAIN || err == eWOULDBLOCK then
            return (-1)
          else
            fromIntegral <$> onError
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

-- | Send data to the socket using sendmsg(2).
sendBufMsg :: SocketAddress sa
           => Socket            -- ^ Socket
           -> sa                -- ^ Destination address
           -> [(Ptr Word8,Int)] -- ^ Data to be sent
           -> [Cmsg]            -- ^ Control messages
           -> MsgFlag           -- ^ Message flags
           -> IO Int            -- ^ The length actually sent
sendBufMsg s sa bufsizs cmsgs flags = do
  sz <- withSocketAddress sa $ \addrPtr addrSize ->
#if !defined(mingw32_HOST_OS)
    withIOVec bufsizs $ \(iovsPtr, iovsLen) -> do
#else
    withWSABuf bufsizs $ \(wsaBPtr, wsaBLen) -> do
#endif
      withCmsgs cmsgs $ \ctrlPtr ctrlLen -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
#if !defined(mingw32_HOST_OS)
              , msgIov     = iovsPtr
              , msgIovLen  = fromIntegral iovsLen
#else
              , msgBuffer    = wsaBPtr
              , msgBufferLen = fromIntegral wsaBLen
#endif
              , msgCtrl    = castPtr ctrlPtr
              , msgCtrlLen = fromIntegral ctrlLen
              , msgFlags   = 0
              }
            cflags = fromMsgFlag flags
        withFdSocket s $ \fd ->
          with msgHdr $ \msgHdrPtr ->
#if defined(mingw32_HOST_OS)
            sendBufMsgWinIO fd msgHdrPtr cflags
#else
            throwSocketErrorWaitWrite s "Network.Socket.Buffer.sendMsg" $
              c_sendmsg fd msgHdrPtr cflags
#endif
  return $ fromIntegral sz

-- | Receive data from the socket using recvmsg(2). The supplied
--   buffers are filled in order, with subsequent buffers used only
--   after all the preceding buffers are full. If the message is short
--   enough some of the supplied buffers may remain unused.
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
#if !defined(mingw32_HOST_OS)
      withIOVec bufsizs $ \(iovsPtr, iovsLen) -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
              , msgIov     = iovsPtr
              , msgIovLen  = fromIntegral iovsLen
              , msgCtrl    = castPtr ctrlPtr
              , msgCtrlLen = fromIntegral clen
              , msgFlags   = 0
#else
      withWSABuf bufsizs $ \(wsaBPtr, wsaBLen) -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
              , msgBuffer    = wsaBPtr
              , msgBufferLen = fromIntegral wsaBLen
              , msgCtrl    = if clen == 0 then nullPtr else castPtr ctrlPtr
              , msgCtrlLen = fromIntegral clen
              , msgFlags   = fromIntegral $ fromMsgFlag flags
#endif
              }
            _cflags = fromMsgFlag flags
        withFdSocket s $ \fd -> do
          with msgHdr $ \msgHdrPtr -> do
            len <-
#if !defined(mingw32_HOST_OS)
                fmap fromIntegral <$>
                throwSocketErrorWaitRead s "Network.Socket.Buffer.recvmsg" $
                      c_recvmsg fd msgHdrPtr _cflags
#else
                recvBufMsgWinIO fd msgHdrPtr
#endif
            sockaddr <- peekSocketAddress addrPtr `catchIOError` \_ -> getPeerName s
            hdr <- peek msgHdrPtr

#if !defined(mingw32_HOST_OS)
            cmsgs <- parseCmsgs msgHdrPtr
            let flags' = MsgFlag $ fromIntegral $ msgFlags hdr
#else
            let flags' = MsgFlag $ fromIntegral (msgFlags hdr)
            -- If the control buffer was truncated (MSG_CTRUNC), the
            -- control data may be invalid and parsing could segfault.
            cmsgs <- if msgCtrl hdr == nullPtr || (msgFlags hdr .&. #{const MSG_CTRUNC}) /= 0
                     then return []
                     else parseCmsgs msgHdrPtr
#endif

            return (sockaddr, len, cmsgs, flags')

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "send"
  c_send :: CSocket -> Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CSocket -> Ptr (MsgHdr sa) -> CInt -> IO CInt -- fixme CSsize
foreign import ccall unsafe "recvmsg"
  c_recvmsg :: CSocket -> Ptr (MsgHdr sa) -> CInt -> IO CInt
#else
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
#endif /* mingw32_HOST_OS */

foreign import ccall unsafe "recv"
  c_recv :: CSocket -> Ptr CChar -> CSize -> CInt -> IO CInt
#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "sendto"
  c_sendto :: CSocket -> Ptr a -> CSize -> CInt -> Ptr sa -> CInt -> IO CInt
foreign import ccall unsafe "recvfrom"
  c_recvfrom :: CSocket -> Ptr a -> CSize -> CInt -> Ptr sa -> Ptr CInt -> IO CInt
#endif
