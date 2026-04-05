{-# LANGUAGE CPP #-}
module Network.Socket.Buffer.WinIO (
    sendBuf,
    sendBufTo,
    recvBuf,
    recvBufFrom,
    recvBufNoWait,
    sendBufMsg,
    recvBufMsg,
) where

#include "HsNet.h"
##include "HsNetDef.h"

import Control.Concurrent.STM
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import GHC.Event.Windows
import GHC.IO.Exception (IOErrorType(InvalidArgument))
import GHC.Windows
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Name (getPeerName)
import Network.Socket.Types (SocketAddress, withSocketAddress, withNewSocketAddress, peekSocketAddress)
import qualified Network.Socket.Types as Generic
import Network.Socket.Types.WinIO
import Network.Socket.Flag
import Network.Socket.Win32.CmsgHdr
import Network.Socket.Win32.MsgHdr
import Network.Socket.Win32.WSABuf (WSABuf(..), withWSABuf)
import Network.Socket.Win32.Load
import System.IO.Error (mkIOError, ioeSetErrorString, catchIOError)
import System.IO.Unsafe (unsafePerformIO)

-- | Extract the byte count from a completed overlapped I/O result.
ioBytes :: IOResult DWORD -> Int
ioBytes = fromIntegral . ioValue

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf s ptr len = withSOCKET s $ \sock ->
    with (WSABuf ptr (fromIntegral len)) $ \wbuf ->
        ioBytes <$> withOverlapped "Network.Socket.sendBuf" sock 0
            (\lpOverlapped -> do
                ret <- c_WSASend sock wbuf 1 nullPtr 0 lpOverlapped nullPtr
                if ret == 0
                    then pure CbPending
                    else do
                        le <- c_getLastError
                        pure $ if le == #{const ERROR_IO_PENDING}
                            then CbPending
                            else CbError (fromIntegral le)
            )
            (\errCode numBytes -> case errCode of
                0 -> ioSuccess numBytes
                _ -> ioFailed $ ntStatusToDosError errCode
            )

sendBufTo :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
sendBufTo s ptr nbytes sa =
    withSocketAddress sa $ \p_sa siz ->
      withSOCKET s $ \sock ->
        with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \wbuf ->
            ioBytes <$> withOverlapped "Network.Socket.sendBufTo" sock 0
                (\lpOverlapped -> do
                    ret <- c_WSASendTo sock wbuf 1 nullPtr 0
                               p_sa (fromIntegral siz) lpOverlapped nullPtr
                    if ret == 0
                        then pure CbPending
                        else do
                            le <- c_getLastError
                            pure $ if le == #{const ERROR_IO_PENDING}
                                then CbPending
                                else CbError (fromIntegral le)
                )
                (\errCode numBytes -> case errCode of
                    0 -> ioSuccess numBytes
                    _ -> ioFailed $ ntStatusToDosError errCode
                )

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf s ptr nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBuf")
    | otherwise = withSOCKET s $ \sock ->
        with (WSABuf ptr (fromIntegral nbytes)) $ \wbuf ->
          alloca $ \flagsPtr -> do
            poke flagsPtr (0 :: DWORD)
            ioBytes <$> withOverlapped "Network.Socket.recvBuf" sock 0
                (\lpOverlapped -> do
                    ret <- c_WSARecv sock wbuf 1 nullPtr flagsPtr lpOverlapped nullPtr
                    if ret == 0
                        then pure CbPending
                        else do
                            le <- c_getLastError
                            pure $ if le == #{const ERROR_IO_PENDING}
                                then CbPending
                                else CbError (fromIntegral le)
                )
                (\errCode numBytes -> case errCode of
                    0 -> ioSuccess numBytes
                    _ -> ioFailed $ ntStatusToDosError errCode
                )

recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFrom s ptr nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvBufFrom")
    | otherwise = withNewSocketAddress $ \ptr_sa sz ->
        alloca $ \ptr_len ->
          withSOCKET s $ \sock ->
            with (WSABuf (castPtr ptr) (fromIntegral nbytes)) $ \wbuf ->
              alloca $ \flagsPtr -> do
                poke ptr_len (fromIntegral sz)
                poke flagsPtr (0 :: DWORD)
                len <- ioBytes <$> withOverlapped "Network.Socket.recvBufFrom" sock 0
                    (\lpOverlapped -> do
                        ret <- c_WSARecvFrom sock wbuf 1 nullPtr flagsPtr
                                   ptr_sa ptr_len lpOverlapped nullPtr
                        if ret == 0
                            then pure CbPending
                            else do
                                le <- c_getLastError
                                pure $ if le == #{const ERROR_IO_PENDING}
                                    then CbPending
                                    else CbError (fromIntegral le)
                    )
                    (\errCode numBytes -> case errCode of
                        0 -> ioSuccess numBytes
                        _ -> ioFailed $ ntStatusToDosError errCode
                    )
                sockaddr <- peekSocketAddress ptr_sa
                    `catchIOError` \_ -> getPeerName (Generic.Socket (Right s))
                return (len, sockaddr)

recvBufNoWait :: Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWait s ptr nbytes = withFdSocket s $ \fd ->
    alloca $ \ptr_bytes -> do
      res <- c_ioctlsocket fd #{const FIONREAD} ptr_bytes
      avail <- peek ptr_bytes
      r <- if res == #{const NO_ERROR} && avail > 0 then
               c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
           else if avail == 0 then
               return (-1)
           else do err <- c_WSAGetLastError
                   if err == #{const WSAEWOULDBLOCK}
                       || err == #{const WSAEINPROGRESS} then
                       return (-1)
                     else
                        return (-2)
      return $ fromIntegral r

-- | Send data to the socket using overlapped WSASendMsg.
sendBufMsg :: SocketAddress sa
           => Socket            -- ^ Socket
           -> sa                -- ^ Destination address
           -> [(Ptr Word8,Int)] -- ^ Data to be sent
           -> [Cmsg]            -- ^ Control messages
           -> MsgFlag           -- ^ Message flags
           -> IO Int            -- ^ The length actually sent
sendBufMsg s sa bufsizs cmsgs flags =
  withSocketAddress sa $ \addrPtr addrSize ->
    withWSABuf bufsizs $ \(wsaBPtr, wsaBLen) ->
      withCmsgs cmsgs $ \ctrlPtr ctrlLen ->
        withSOCKET s $ \sock -> do
          let msgHdr = MsgHdr {
                  msgName    = addrPtr
                , msgNameLen = fromIntegral addrSize
                , msgBuffer    = wsaBPtr
                , msgBufferLen = fromIntegral wsaBLen
                , msgCtrl    = castPtr ctrlPtr
                , msgCtrlLen = fromIntegral ctrlLen
                , msgFlags   = 0
                }
              cflags = fromIntegral $ fromMsgFlag flags
          wsaSendMsg <- loadWSASendMsg sock
          with msgHdr $ \msgHdrPtr ->
            ioBytes <$> withOverlapped "Network.Socket.sendBufMsg" sock 0
                (\lpOverlapped -> do
                    ret <- wsaSendMsg sock msgHdrPtr cflags nullPtr lpOverlapped nullPtr
                    if ret == 0
                        then pure CbPending
                        else do
                            le <- c_getLastError
                            pure $ if le == #{const ERROR_IO_PENDING}
                                then CbPending
                                else CbError (fromIntegral le)
                )
                (\errCode numBytes -> case errCode of
                    0 -> ioSuccess numBytes
                    _ -> ioFailed $ ntStatusToDosError errCode
                )

-- | Receive data from the socket using overlapped WSARecvMsg.
recvBufMsg :: SocketAddress sa
           => Socket            -- ^ Socket
           -> [(Ptr Word8,Int)] -- ^ A list of (buffer, buffer-length) pairs.
           -> Int               -- ^ The buffer size for control messages.
           -> MsgFlag           -- ^ Message flags
           -> IO (sa,Int,[Cmsg],MsgFlag) -- ^ Source address, total bytes received, control messages and message flags
recvBufMsg s bufsizs clen flags =
  withNewSocketAddress $ \addrPtr addrSize ->
    allocaBytes clen $ \ctrlPtr ->
      withWSABuf bufsizs $ \(wsaBPtr, wsaBLen) ->
        withSOCKET s $ \sock -> do
          let msgHdr = MsgHdr {
                  msgName    = addrPtr
                , msgNameLen = fromIntegral addrSize
                , msgBuffer    = wsaBPtr
                , msgBufferLen = fromIntegral wsaBLen
                , msgCtrl    = if clen == 0 then nullPtr else castPtr ctrlPtr
                , msgCtrlLen = fromIntegral clen
                , msgFlags   = fromIntegral $ fromMsgFlag flags
                }
          wsaRecvMsg <- loadWSARecvMsg sock
          with msgHdr $ \msgHdrPtr -> do
            len <- ioBytes <$> withOverlapped "Network.Socket.recvBufMsg" sock 0
                (\lpOverlapped -> do
                    ret <- wsaRecvMsg sock msgHdrPtr nullPtr lpOverlapped nullPtr
                    if ret == 0
                        then pure CbPending
                        else do
                            le <- c_getLastError
                            -- WSAEMSGSIZE means a truncated message completed
                            -- synchronously, but an IOCP completion is still
                            -- queued.  Treat it as pending so the completion
                            -- is consumed normally (it arrives as
                            -- STATUS_BUFFER_OVERFLOW / ERROR_MORE_DATA).
                            pure $ if le == #{const ERROR_IO_PENDING}
                                       || le == #{const WSAEMSGSIZE}
                                then CbPending
                                else CbError (fromIntegral le)
                )
                (\errCode numBytes -> if errCode == 0
                    then ioSuccess numBytes
                    else
                        -- Truncated message (WSAEMSGSIZE) — not an error,
                        -- the data was received but the buffer was too small.
                        let wserr = ntStatusToDosError errCode
                            in if wserr == #{const ERROR_MORE_DATA}
                                then ioSuccess numBytes
                                else ioFailed wserr
                )
            sockaddr <- peekSocketAddress addrPtr
                `catchIOError` \_ -> getPeerName (Generic.Socket (Right s))
            hdr <- peek msgHdrPtr
            let flags' = MsgFlag $ fromIntegral $ msgFlags hdr
            -- When control messages were truncated, Control.len may
            -- report the needed size rather than the actual buffer
            -- size, causing cmsg_nxthdr to read past our buffer.
            cmsgs' <- if flags' .&. MSG_CTRUNC /= mempty
                then pure []
                else parseCmsgs msgHdrPtr
            return (sockaddr, len, cmsgs', flags')

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

-- | Convert an NTSTATUS code to a Win32 error code.
foreign import ccall unsafe "RtlNtStatusToDosError"
    ntStatusToDosError :: ErrCode -> ErrCode

foreign import CALLCONV unsafe "WSASend"
    c_WSASend :: SOCKET -> Ptr WSABuf -> DWORD -> Ptr DWORD -> DWORD
              -> LPOVERLAPPED -> Ptr () -> IO CInt

foreign import CALLCONV unsafe "WSARecv"
    c_WSARecv :: SOCKET -> Ptr WSABuf -> DWORD -> Ptr DWORD -> Ptr DWORD
              -> LPOVERLAPPED -> Ptr () -> IO CInt

foreign import CALLCONV unsafe "WSASendTo"
    c_WSASendTo :: SOCKET -> Ptr WSABuf -> DWORD -> Ptr DWORD -> DWORD
                -> Ptr sa -> CInt -> LPOVERLAPPED -> Ptr () -> IO CInt

foreign import CALLCONV unsafe "WSARecvFrom"
    c_WSARecvFrom :: SOCKET -> Ptr WSABuf -> DWORD -> Ptr DWORD -> Ptr DWORD
                  -> Ptr sa -> Ptr CInt -> LPOVERLAPPED -> Ptr () -> IO CInt

-- WSASendMsg / WSARecvMsg are Winsock extension functions that must be
-- loaded at runtime via WSAIoctl + SIO_GET_EXTENSION_FUNCTION_POINTER.
-- The C loaders live in cbits/winSock.c.

type WSASendMsgFn sa =
    SOCKET -> Ptr (MsgHdr sa) -> DWORD -> Ptr DWORD
    -> LPOVERLAPPED -> Ptr () -> IO CInt

type WSARecvMsgFn sa =
    SOCKET -> Ptr (MsgHdr sa) -> Ptr DWORD
    -> LPOVERLAPPED -> Ptr () -> IO CInt

foreign import ccall unsafe "dynamic"
    mkWSASendMsg :: FunPtr (WSASendMsgFn sa) -> WSASendMsgFn sa

foreign import ccall unsafe "dynamic"
    mkWSARecvMsg :: FunPtr (WSARecvMsgFn sa) -> WSARecvMsgFn sa

foreign import ccall unsafe "loadWSASendMsg"
    c_loadWSASendMsg :: SOCKET -> IO (FunPtr (WSASendMsgFn sa))

foreign import ccall unsafe "loadWSARecvMsg"
    c_loadWSARecvMsg :: SOCKET -> IO (FunPtr (WSARecvMsgFn sa))

wsaSendMsgPtr :: TVar (Loaded a)
wsaSendMsgPtr = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE wsaSendMsgPtr #-}

wsaRecvMsgPtr :: TVar (Loaded a)
wsaRecvMsgPtr = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE wsaRecvMsgPtr #-}

loadWSASendMsg :: SOCKET -> IO (WSASendMsgFn sa)
loadWSASendMsg = loadExtensionFunction wsaSendMsgPtr c_loadWSASendMsg mkWSASendMsg "WSASendMsg"

loadWSARecvMsg :: SOCKET -> IO (WSARecvMsgFn sa)
loadWSARecvMsg = loadExtensionFunction wsaRecvMsgPtr c_loadWSARecvMsg mkWSARecvMsg "WSARecvMsg"

foreign import CALLCONV unsafe "ioctlsocket"
    c_ioctlsocket :: CInt -> CLong -> Ptr CULong -> IO CInt

foreign import CALLCONV unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

foreign import CALLCONV unsafe "WSAGetLastError"
    c_WSAGetLastError :: IO CInt
