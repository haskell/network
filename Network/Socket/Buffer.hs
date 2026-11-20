{-# LANGUAGE CPP #-}

-- | Sending and receiving with buffers.  The work is done by
--   "Network.Socket.Posix.Buffer" or "Network.Socket.Win32.Buffer".
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

import GHC.IO.Exception (IOErrorType(InvalidArgument))
import System.IO.Error (mkIOError, ioeSetErrorString)

import Network.Socket.Flag
import Network.Socket.Imports
import Network.Socket.Types
#if defined(mingw32_HOST_OS)
import Network.Socket.Win32.Cmsg (Cmsg)
import qualified Network.Socket.Win32.Buffer as P
#else
import Network.Socket.Posix.Cmsg (Cmsg)
import qualified Network.Socket.Posix.Buffer as P
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
sendBufTo = P.sendBufTo

-- | Send data to the socket. The socket must be connected to a remote
-- socket. Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
sendBuf :: Socket    -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> IO Int     -- Number of Bytes sent
sendBuf = P.sendBuf

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
    | otherwise   = P.recvBufFrom s ptr nbytes

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
    | otherwise   = P.recvBuf s ptr nbytes

-- | Receive data from the socket. This function returns immediately
--   even if data is not available. In other words, IO manager is NOT
--   involved. The length of data is returned if received.
--   -1 is returned in the case of EAGAIN or EWOULDBLOCK.
--   -2 is returned in other error cases.
--
--   @since 3.2.9.0
recvBufNoWait :: Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWait = recvBufNoWaitOr $ return (-2)

-- | 'recvBufNoWait' which runs the action on an error, in place of
--   answering -2.  The error is gone from the socket once read, so this
--   is where to raise it.
recvBufNoWaitOr :: IO CInt -> Socket -> Ptr Word8 -> Int -> IO Int
recvBufNoWaitOr = P.recvBufNoWaitOr

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
sendBufMsg = P.sendBufMsg

-- | Receive data from the socket using recvmsg(2). The supplied
--   buffers are filled in order, with subsequent buffers used only
--   after all the preceding buffers are full. If the message is short
--   enough some of the supplied buffers may remain unused.
--   Descriptors received in control messages are close-on-exec, as the
--   sockets network creates are (atomically where @MSG_CMSG_CLOEXEC@ exists,
--   right after the receive elsewhere, such as on macOS).
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
recvBufMsg = P.recvBufMsg
