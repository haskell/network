{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

##include "HsNetDef.h"

module Network.Socket.Posix.Buffer (
    sendBufTo
  , sendBuf
  , recvBufFrom
  , recvBuf
  , recvBufNoWaitOr
  , sendBufMsg
  , recvBufMsg
  ) where

import Foreign.C.Error (getErrno, eAGAIN, eWOULDBLOCK)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import System.IO.Error (catchIOError)
import System.Posix.Types (Fd (..))

import Network.Socket.Cbits (msgCmsgCloexec)
import Network.Socket.Posix.Cmsg
import Network.Socket.Posix.CmsgHdr
import Network.Socket.Posix.Fcntl (setCloseOnExecIfNeeded)
import Network.Socket.Posix.MsgHdr
import Network.Socket.Posix.IOVec

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Name
import Network.Socket.Types
import Network.Socket.Flag

sendBufTo :: SocketAddress sa => Socket -> Ptr a -> Int -> sa -> IO Int
sendBufTo s ptr nbytes sa =
  withSocketAddress sa $ \p_sa siz -> fromIntegral <$> do
    withFdSocket s $ \fd -> do
        let sz = fromIntegral siz
            n = fromIntegral nbytes
            flags = 0
        throwSocketErrorWaitWrite s "Network.Socket.sendBufTo" $
          c_sendto fd ptr n flags p_sa sz

sendBuf :: Socket    -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> IO Int     -- Number of Bytes sent
sendBuf s str len =
    fromIntegral <$> withFdSocket s (\fd -> do
        let flags = 0
            clen = fromIntegral len
        throwSocketErrorWaitWrite s "Network.Socket.sendBuf" $
          c_send fd str clen flags)

recvBufFrom :: SocketAddress sa => Socket -> Ptr a -> Int -> IO (Int, sa)
recvBufFrom s ptr nbytes =
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

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf s ptr nbytes = do
    len <- withFdSocket s $ \fd ->
        throwSocketErrorWaitRead s "Network.Socket.recvBuf" $
             c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    return $ fromIntegral len

recvBufNoWaitOr :: IO CInt -> Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWaitOr onError s ptr nbytes = withFdSocket s $ \fd -> do
    r <- c_recv fd (castPtr ptr) (fromIntegral nbytes) 0{-flags-}
    if r >= 0 then
        return $ fromIntegral r
      else do
        err <- getErrno
        if err == eAGAIN || err == eWOULDBLOCK then
            return (-1)
          else
            fromIntegral <$> onError

sendBufMsg :: SocketAddress sa
           => Socket            -- ^ Socket
           -> sa                -- ^ Destination address
           -> [(Ptr Word8,Int)] -- ^ Data to be sent
           -> [Cmsg]            -- ^ Control messages
           -> MsgFlag           -- ^ Message flags
           -> IO Int            -- ^ The length actually sent
sendBufMsg s sa bufsizs cmsgs flags = do
  sz <- withSocketAddress sa $ \addrPtr addrSize ->
    withIOVec bufsizs $ \(iovsPtr, iovsLen) -> do
      withCmsgs cmsgs $ \ctrlPtr ctrlLen -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
              , msgIov     = iovsPtr
              , msgIovLen  = fromIntegral iovsLen
              , msgCtrl    = castPtr ctrlPtr
              , msgCtrlLen = fromIntegral ctrlLen
              , msgFlags   = 0
              }
            cflags = fromMsgFlag flags
        withFdSocket s $ \fd ->
          with msgHdr $ \msgHdrPtr ->
            throwSocketErrorWaitWrite s "Network.Socket.Buffer.sendMsg" $
              c_sendmsg fd msgHdrPtr cflags
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
      withIOVec bufsizs $ \(iovsPtr, iovsLen) -> do
        let msgHdr = MsgHdr {
                msgName    = addrPtr
              , msgNameLen = fromIntegral addrSize
              , msgIov     = iovsPtr
              , msgIovLen  = fromIntegral iovsLen
              , msgCtrl    = castPtr ctrlPtr
              , msgCtrlLen = fromIntegral clen
              , msgFlags   = 0
              }
            -- Received descriptors are close-on-exec, as network's own are.
            cflags = fromMsgFlag flags .|. msgCmsgCloexec
        withFdSocket s $ \fd -> do
          with msgHdr $ \msgHdrPtr -> do
            len <-
                fmap fromIntegral <$>
                throwSocketErrorWaitRead s "Network.Socket.Buffer.recvmsg" $
                      c_recvmsg fd msgHdrPtr cflags
            sockaddr <- peekSocketAddress addrPtr `catchIOError` \_ -> getPeerName s
            hdr <- peek msgHdrPtr

            cmsgs <- parseCmsgs msgHdrPtr
            -- Without MSG_CMSG_CLOEXEC, marked here: later than the kernel
            -- would, but before the caller could.
            when (msgCmsgCloexec == 0) $
                forM_ (receivedFds cmsgs) setCloseOnExecIfNeeded
            -- Linux echoes MSG_CMSG_CLOEXEC, which the caller did not ask for.
            let flags' = MsgFlag $ fromIntegral (msgFlags hdr) .&. complement msgCmsgCloexec

            return (sockaddr, len, cmsgs, flags')

-- | The descriptors in SCM_RIGHTS control messages.
receivedFds :: [Cmsg] -> [CInt]
receivedFds cmsgs =
    [ fd | c <- cmsgs, cmsgId c == CmsgIdFds
         , Just fds <- [decodeCmsg c :: Maybe [Fd]], Fd fd <- fds ]

foreign import ccall unsafe "send"
  c_send :: CSocket -> Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CSocket -> Ptr (MsgHdr sa) -> CInt -> IO CInt -- fixme CSsize
foreign import ccall unsafe "recvmsg"
  c_recvmsg :: CSocket -> Ptr (MsgHdr sa) -> CInt -> IO CInt

foreign import ccall unsafe "recv"
  c_recv :: CSocket -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "sendto"
  c_sendto :: CSocket -> Ptr a -> CSize -> CInt -> Ptr sa -> CInt -> IO CInt
foreign import ccall unsafe "recvfrom"
  c_recvfrom :: CSocket -> Ptr a -> CSize -> CInt -> Ptr sa -> Ptr CInt -> IO CInt
