{-# LANGUAGE RecordWildCards #-}

#include "HsNet.h"

-- |
-- Module      : Network.Socket.ByteString
-- Copyright   : (c) Johan Tibell 2007-2010
-- License     : BSD-style
--
-- Maintainer  : johan.tibell@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- This module provides access to the BSD /socket/ interface.  This
-- module is generally more efficient than the 'String' based network
-- functions in 'Network.Socket'.  For detailed documentation, consult
-- your favorite POSIX socket reference. All functions communicate
-- failures by converting the error number to 'System.IO.IOError'.
--
-- This module is made to be imported with 'Network.Socket' like so:
--
-- > import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- > import Network.Socket.ByteString
--
module Network.Socket.ByteString
    (
    -- * Send data to a socket
      send
    , sendAll
    , sendTo
    , sendAllTo

    -- ** Vectored I/O
    -- $vectored
    , sendMany
    , sendManyTo

    -- * Receive data from a socket
    , recv
    , recvFrom
    ) where

import Control.Exception as E (catch, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Network.Socket (sendBuf, sendBufTo, recvBuf, recvBufFrom)
import System.IO.Error (isEOFError)

import Network.Socket.ByteString.Internal
import Network.Socket.Types

#if !defined(mingw32_HOST_OS)
import Control.Monad (zipWithM_)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import Network.Socket.Internal

import Network.Socket.ByteString.IOVec (IOVec(..))
import Network.Socket.ByteString.MsgHdr (MsgHdr(..))
#endif

-- ----------------------------------------------------------------------------
-- Sending

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
send :: Socket     -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Number of bytes sent
send s xs = unsafeUseAsCStringLen xs $ \(str, len) ->
    sendBuf s (castPtr str) len

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Unlike 'send', this function continues to send data
-- until either all data has been sent or an error occurs.  On error,
-- an exception is raised, and there is no way to determine how much
-- data, if any, was successfully sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
sendAll :: Socket     -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll s bs = do
    sent <- send s bs
    when (sent < B.length bs) $ sendAll s (B.drop sent bs)

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for
-- ensuring that all data has been sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
sendTo :: Socket     -- ^ Socket
       -> ByteString  -- ^ Data to send
       -> SockAddr    -- ^ Recipient address
       -> IO Int      -- ^ Number of bytes sent
sendTo s xs addr =
    unsafeUseAsCStringLen xs $ \(str, len) -> sendBufTo s str len addr

-- | Send data to the socket. The recipient can be specified
-- explicitly, so the socket need not be in a connected state.  Unlike
-- 'sendTo', this function continues to send data until either all
-- data has been sent or an error occurs.  On error, an exception is
-- raised, and there is no way to determine how much data, if any, was
-- successfully sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
sendAllTo :: Socket     -- ^ Socket
          -> ByteString  -- ^ Data to send
          -> SockAddr    -- ^ Recipient address
          -> IO ()
sendAllTo s xs addr = do
    sent <- sendTo s xs addr
    when (sent < B.length xs) $ sendAllTo s (B.drop sent xs) addr

-- ----------------------------------------------------------------------------
-- ** Vectored I/O

-- $vectored
--
-- Vectored I\/O, also known as scatter\/gather I\/O, allows multiple
-- data segments to be sent using a single system call, without first
-- concatenating the segments.  For example, given a list of
-- @ByteString@s, @xs@,
--
-- > sendMany sock xs
--
-- is equivalent to
--
-- > sendAll sock (concat xs)
--
-- but potentially more efficient.
--
-- Vectored I\/O are often useful when implementing network protocols
-- that, for example, group data into segments consisting of one or
-- more fixed-length headers followed by a variable-length body.

-- | Send data to the socket.  The socket must be in a connected
-- state.  The data is sent as if the parts have been concatenated.
-- This function continues to send data until either all data has been
-- sent or an error occurs.  On error, an exception is raised, and
-- there is no way to determine how much data, if any, was
-- successfully sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
sendMany :: Socket       -- ^ Connected socket
         -> [ByteString]  -- ^ Data to send
         -> IO ()
#if !defined(mingw32_HOST_OS)
sendMany s cs = do
    sent <- sendManyInner
    when (sent < totalLength cs) $ sendMany s (remainingChunks sent cs)
  where
    sendManyInner =
      fmap fromIntegral . withIOVec cs $ \(iovsPtr, iovsLen) ->
          throwSocketErrorWaitWrite s "Network.Socket.ByteString.sendMany" $
              c_writev (fromIntegral s) iovsPtr
              (fromIntegral (min iovsLen (#const IOV_MAX)))
#else
sendMany s = sendAll s . B.concat
#endif

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.  The
-- data is sent as if the parts have been concatenated.  This function
-- continues to send data until either all data has been sent or an
-- error occurs.  On error, an exception is raised, and there is no
-- way to determine how much data, if any, was successfully sent.
--
-- Sending data to closed socket may lead to undefined behaviour.
sendManyTo :: Socket       -- ^ Socket
           -> [ByteString]  -- ^ Data to send
           -> SockAddr      -- ^ Recipient address
           -> IO ()
#if !defined(mingw32_HOST_OS)
sendManyTo s cs addr = do
    sent <- fmap fromIntegral sendManyToInner
    when (sent < totalLength cs) $ sendManyTo s (remainingChunks sent cs) addr
  where
    sendManyToInner =
      withSockAddr addr $ \addrPtr addrSize ->
        withIOVec cs $ \(iovsPtr, iovsLen) -> do
          let msgHdr = MsgHdr
                addrPtr (fromIntegral addrSize)
                iovsPtr (fromIntegral iovsLen)
          with msgHdr $ \msgHdrPtr ->
            throwSocketErrorWaitWrite s "Network.Socket.ByteString.sendManyTo" $
              c_sendmsg (fromIntegral s) msgHdrPtr 0
#else
sendManyTo s cs = sendAllTo s (B.concat cs)
#endif

-- ----------------------------------------------------------------------------
-- Receiving

-- | Receive data from the socket.  The socket must be in a connected
-- state.  This function may return fewer bytes than specified.  If
-- the message is longer than the specified length, it may be
-- discarded depending on the type of socket.  This function may block
-- until a message arrives.
--
-- Considering hardware and network realities, the maximum number of bytes to
-- receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has
-- closed its half side of the connection.
--
-- Receiving data from closed socket may lead to undefined behaviour.
recv :: Socket        -- ^ Connected socket
     -> Int            -- ^ Maximum number of bytes to receive
     -> IO ByteString  -- ^ Data received
recv s nbytes
    | nbytes < 0 = ioError (mkInvalidRecvArgError "Network.Socket.ByteString.recv")
    | otherwise  = createAndTrim nbytes $ \ptr ->
        E.catch
          (recvBuf s ptr nbytes)
          (\e -> if isEOFError e then return 0 else throwIO e)

-- | Receive data from the socket.  The socket need not be in a
-- connected state.  Returns @(bytes, address)@ where @bytes@ is a
-- 'ByteString' representing the data received and @address@ is a
-- 'SockAddr' representing the address of the sending socket.
--
-- Receiving data from closed socket may lead to undefined behaviour.
recvFrom :: Socket                     -- ^ Socket
         -> Int                        -- ^ Maximum number of bytes to receive
         -> IO (ByteString, SockAddr)  -- ^ Data received and sender address
recvFrom sock nbytes =
    allocaBytes nbytes $ \ptr -> do
        (len, sockaddr) <- recvBufFrom sock ptr nbytes
        str <- B.packCStringLen (ptr, len)
        return (str, sockaddr)

-- ----------------------------------------------------------------------------
-- Not exported

#if !defined(mingw32_HOST_OS)
-- | Suppose we try to transmit a list of chunks @cs@ via a gathering write
-- operation and find that @n@ bytes were sent. Then @remainingChunks n cs@ is
-- list of chunks remaining to be sent.
remainingChunks :: Int -> [ByteString] -> [ByteString]
remainingChunks _ [] = []
remainingChunks i (x:xs)
    | i < len        = B.drop i x : xs
    | otherwise      = let i' = i - len in i' `seq` remainingChunks i' xs
  where
    len = B.length x

-- | @totalLength cs@ is the sum of the lengths of the chunks in the list @cs@.
totalLength :: [ByteString] -> Int
totalLength = sum . map B.length

-- | @withIOVec cs f@ executes the computation @f@, passing as argument a pair
-- consisting of a pointer to a temporarily allocated array of pointers to
-- IOVec made from @cs@ and the number of pointers (@length cs@).
-- /Unix only/.
withIOVec :: [ByteString] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVec cs f =
    allocaArray csLen $ \aPtr -> do
        zipWithM_ pokeIov (ptrs aPtr) cs
        f (aPtr, csLen)
  where
    csLen = length cs
    ptrs = iterate (`plusPtr` sizeOf (undefined :: IOVec))
    pokeIov ptr s =
        unsafeUseAsCStringLen s $ \(sPtr, sLen) ->
        poke ptr $ IOVec sPtr (fromIntegral sLen)
#endif
