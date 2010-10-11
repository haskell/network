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
-- This module provides access to the BSD /socket/ interface.  This
-- module is generally more efficient than the 'String' based network
-- functions in 'Network.Socket'.  For detailed documentation, consult
-- your favorite POSIX socket reference. All functions communicate
-- failures by converting the error number to 'System.IO.IOError'.
--
-- This module is made to be imported with 'Network.Socket' like so:
--
-- > import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- > import Network.Socket.ByteString.Lazy
-- > import Prelude hiding (getContents)
--
module Network.Socket.ByteString.Lazy
  (
#if !defined(mingw32_HOST_OS)
    -- * Send data to a socket
      send,
      sendAll,
#endif

    -- * Receive data from a socket
      getContents,
      recv
  ) where

import Control.Monad (liftM)
import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import Data.Int (Int64)
import Network.Socket (Socket(..), ShutdownCmd(..), shutdown)
import Prelude hiding (getContents)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString as S
import qualified Network.Socket.ByteString as N

#if !defined(mingw32_HOST_OS)
import Control.Monad (unless)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import Network.Socket.ByteString.IOVec (IOVec(IOVec))
import Network.Socket.ByteString.Internal (c_writev)
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)

import qualified Data.ByteString.Lazy as L

#  if defined(__GLASGOW_HASKELL__)
import GHC.Conc (threadWaitWrite)
#  endif
#endif

#if !defined(mingw32_HOST_OS)
-- -----------------------------------------------------------------------------
-- Sending

-- | Send data to the socket. The socket must be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for
-- ensuring that all data has been sent.
--
-- Because a lazily generated 'ByteString' may be arbitrarily long,
-- this function caps the amount it will attempt to send at 4MB.  This
-- number is large (so it should not penalize performance on fast
-- networks), but not outrageously so (to avoid demanding lazily
-- computed data unnecessarily early).  Before being sent, the lazy
-- 'ByteString' will be converted to a list of strict 'ByteString's
-- with 'L.toChunks'; at most 1024 chunks will be sent.  /Unix only/.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int64    -- ^ Number of bytes sent
send (MkSocket fd _ _ _ _) s = do
  let cs  = take maxNumChunks (L.toChunks s)
      len = length cs
  liftM fromIntegral . allocaArray len $ \ptr ->
    withPokes cs ptr $ \niovs ->
#  if !defined(__HUGS__)
      throwSocketErrorIfMinus1RetryMayBlock "writev"
        (threadWaitWrite (fromIntegral fd)) $
#  endif
        c_writev (fromIntegral fd) ptr niovs
  where
    withPokes ss p f = loop ss p 0 0
      where loop (c:cs) q k !niovs
                | k < maxNumBytes =
                    unsafeUseAsCStringLen c $ \(ptr,len) -> do
                      poke q $ IOVec ptr (fromIntegral len)
                      loop cs (q `plusPtr` sizeOf (undefined :: IOVec))
                              (k + fromIntegral len) (niovs + 1)
                | otherwise = f niovs
            loop _ _ _ niovs = f niovs
    maxNumBytes  = 4194304 :: Int  -- maximum number of bytes to transmit in one system call
    maxNumChunks = 1024    :: Int  -- maximum number of chunks to transmit in one system call

-- | Send data to the socket.  The socket must be in a connected
-- state. This function continues to send data until either all data
-- has been sent or an error occurs.  If there is an error, an
-- exception is raised, and there is no way to determine how much data
-- was sent.  /Unix only/.
sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
  sent <- send sock bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll sock bs'
#endif

-- -----------------------------------------------------------------------------
-- Receiving

-- | Receive data from the socket.  The socket must be in a connected
-- state.  Data is received on demand, in chunks; each chunk will be
-- sized to reflect the amount of data received by individual 'recv'
-- calls.
--
-- All remaining data from the socket is consumed.  When there is no
-- more data to be received, the receiving side of the socket is shut
-- down.  If there is an error and an exception is thrown, the socket
-- is not shut down.
getContents :: Socket         -- ^ Connected socket
            -> IO ByteString  -- ^ Data received
getContents sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock defaultChunkSize
    if S.null s
      then shutdown sock ShutdownReceive >> return Empty
      else Chunk s `liftM` loop

-- | Receive data from the socket.  The socket must be in a connected
-- state.  This function may return fewer bytes than specified.  If
-- the received data is longer than the specified length, it may be
-- discarded depending on the type of socket.  This function may block
-- until a message arrives.
--
-- If there is no more data to be received, returns an empty 'ByteString'.
recv :: Socket         -- ^ Connected socket
     -> Int64          -- ^ Maximum number of bytes to receive
     -> IO ByteString  -- ^ Data received
recv sock nbytes = chunk `liftM` N.recv sock (fromIntegral nbytes) where
  chunk k
    | S.null k  = Empty
    | otherwise = Chunk k Empty
