module Network.Socket.String where

import Foreign.C.String (peekCStringLen, withCStringLen)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)

import Network.Socket.Buffer
import Network.Socket.Types

-----------------------------------------------------------------------------
-- ** Sending and receiving data

-- $sendrecv
--
-- Do not use the @send@ and @recv@ functions defined in this section
-- in new code, as they incorrectly represent binary data as a Unicode
-- string.  As a result, these functions are inefficient and may lead
-- to bugs in the program.  Instead use the @send@ and @recv@
-- functions defined in the "Network.Socket.ByteString" module.

-----------------------------------------------------------------------------
-- sendTo & recvFrom

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
{-# DEPRECATED sendTo "Use sendTo defined in \"Network.Socket.ByteString\"" #-}
sendTo :: Socket        -- (possibly) bound/connected Socket
       -> String        -- Data to send
       -> SockAddr
       -> IO Int        -- Number of Bytes sent
sendTo sock xs addr = do
 withCStringLen xs $ \(str, len) -> do
   sendBufTo sock str len addr


-- | Receive data from the socket. The socket need not be in a
-- connected state. Returns @(bytes, nbytes, address)@ where @bytes@
-- is a @String@ of length @nbytes@ representing the data received and
-- @address@ is a 'SockAddr' representing the address of the sending
-- socket.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
{-# DEPRECATED recvFrom "Use recvFrom defined in \"Network.Socket.ByteString\"" #-}
recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock nbytes =
  allocaBytes nbytes $ \ptr -> do
    (len, sockaddr) <- recvBufFrom sock ptr nbytes
    str <- peekCStringLen (ptr, len)
    return (str, len, sockaddr)


-----------------------------------------------------------------------------
-- send & recv

-- | Send data to the socket. The socket must be connected to a remote
-- socket. Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
{-# DEPRECATED send "Use send defined in \"Network.Socket.ByteString\"" #-}
send :: Socket  -- Bound/Connected Socket
     -> String  -- Data to send
     -> IO Int  -- Number of Bytes sent
send sock xs = withCStringLen xs $ \(str, len) ->
    sendBuf sock (castPtr str) len

-- | Receive data from the socket.  The socket must be in a connected
-- state. This function may return fewer bytes than specified.  If the
-- message is longer than the specified length, it may be discarded
-- depending on the type of socket.  This function may block until a
-- message arrives.
--
-- Considering hardware and network realities, the maximum number of
-- bytes to receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has
-- closed its half side of the connection.
--
-- Receiving data from closed socket may lead to undefined behaviour.
{-# DEPRECATED recv "Use recv defined in \"Network.Socket.ByteString\"" #-}
recv :: Socket -> Int -> IO String
recv sock l = fst `fmap` recvLen sock l

{-# DEPRECATED recvLen "Use recv defined in \"Network.Socket.ByteString\" with \"Data.Bytestring.length\"" #-}
recvLen :: Socket -> Int -> IO (String, Int)
recvLen sock nbytes =
     allocaBytes nbytes $ \ptr -> do
        len <- recvBuf sock ptr nbytes
        s <- peekCStringLen (castPtr ptr,len)
        return (s, len)
