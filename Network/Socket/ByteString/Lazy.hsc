{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Network.Socket.ByteString.Lazy
-- Copyright   : (c) Bryan O'Sullivan 2009
-- License     : BSD-style
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- A module for efficiently transmitting data over sockets. For
-- detailed documentation consult your favorite POSIX socket
-- reference. All functions communicate failures by converting the
-- error number to an 'System.IO.IOError'.
--
-- This module is intended to be imported together with
-- 'Network.Socket' like so:
--
-- > import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- > import Network.Socket.ByteString.Lazy
-- > import Prelude hiding (getContents)
--
-- Alternatively, you can import it qualified.
--
-- > import qualified Network.Socket.ByteString.Lazy as S
module Network.Socket.ByteString.Lazy
    (
    -- * Send data on a socket
      sendAll
    , send
    -- * Receive data on a socket
    , recv
    , recv_
    , getContents
    ) where

import Control.Monad (liftM)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import Data.Int (Int64)
import Foreign.Marshal.Array (allocaArray)
import qualified Network.Socket.ByteString as N
import Network.Socket.ByteString.Internal
import Network.Socket.ByteString.IOVec
import Network.Socket (Socket(..), ShutdownCmd(..), shutdown)
import System.IO.Unsafe (unsafeInterleaveIO)
import Prelude hiding (getContents)
import GHC.Conc (threadWaitWrite)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (plusPtr)

-- | Send a 'ByteString' using a single system call.
--
-- Because a lazily generated 'ByteString' may be arbitrarily long,
-- this function caps the amount it will attempt to send at 4MB.  This
-- number is large (so it should not penalize performance on fast
-- networks), but not outrageously so (to avoid demanding lazily
-- computed data unnecessarily early).
send :: Socket -> ByteString -> IO Int64
send (MkSocket fd _ _ _ _) s = do
    let cs = L.toChunks s
        len = length cs
    liftM fromIntegral . allocaArray len $ \ptr ->
      withPokes cs ptr $ \niovs ->
        throwErrnoIfMinus1Retry_repeatOnBlock "writev"
          (threadWaitWrite (fromIntegral fd)) $
          c_writev (fromIntegral fd) ptr niovs
  where
    withPokes ss p f = loop ss p 0 0
      where loop (c:cs) q k !niovs
                | k < sendLimit =
                    unsafeUseAsCStringLen c $ \(ptr,len) -> do
                      let iov = IOVec ptr (fromIntegral len)
                      poke q iov
                      loop cs (q `plusPtr` sizeOf iov)
                              (k + fromIntegral len) (niovs + 1)
                | otherwise = f niovs
            loop _ _ _ niovs = f niovs
    -- Limit the amount of data that we'll try to transmit with a
    -- single system call.
    sendLimit = 4194304 :: Int

-- | Send the entire contents of a string, possibly using multiple
-- 'send' system calls to do so.
sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs = do
  sent <- send sock bs
  if sent < L.length bs
    then sendAll sock (L.drop sent bs)
    else return ()

-- | Lazily receive 'ByteString' data, in chunks. Chunks are received
-- on demand; each chunk will be sized to reflect the amount of data
-- received by individual 'recv_' calls.
--
-- All remaining data from the socket is consumed. The receiving side
-- of the socket is shut down when there is no more data to be
-- received.  The socket is not shut down if an exception is thrown.
getContents :: Socket -> IO ByteString
getContents sock = loop
  where loop = unsafeInterleaveIO $ do
          s <- N.recv_ sock defaultChunkSize
          if S.null s
            then shutdown sock ShutdownReceive >> return Empty
            else Chunk s `liftM` loop

-- | Receive a message. The socket must be in a connected state so
-- that the intended recipient is known. Note that the length of the
-- received data can be smaller than specified maximum length. If the
-- message is longer than the specified length it may be discarded
-- depending on the type of socket. May block until a message arrives.
--
-- When the end of the input stream is reached, returns an empty
-- 'ByteString'.
recv_ :: Socket -> Int64 -> IO ByteString
recv_ sock nbytes = chunk `liftM` N.recv_ sock (fromIntegral nbytes)
    where chunk k | S.null k  = Empty
                  | otherwise = Chunk k Empty

-- | Receive a message from another socket. Similar to 'recv_', but
-- throws an EOF exception at end of input.
recv :: Socket -> Int64 -> IO ByteString
recv sock nbytes = chunk `liftM` N.recv sock (fromIntegral nbytes)
    where chunk k = Chunk k Empty
