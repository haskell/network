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
-- A module for efficiently transmitting data over sockets. For detailed
-- documentation, consult your favorite POSIX socket reference. All functions
-- communicate failures by converting the error number to 'System.IO.IOError'.
--
-- This module is made to be imported with 'Network.Socket' like so:
--
-- > import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- > import Network.Socket.ByteString.Lazy
-- > import Prelude hiding (getContents)
--
module Network.Socket.ByteString.Lazy
  ( -- * Send messages on sockets
    -- | Functions for sending messages on sockets
    send
  , sendAll

    -- * Receive messages from sockets
    -- | Functions for receiving messages from sockets
  , getContents
  , recv
  ) where

import Control.Monad (liftM, when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import GHC.Conc (threadWaitWrite)
import qualified Network.Socket.ByteString as N
import Network.Socket (Socket(..), ShutdownCmd(..), shutdown)
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)
import Network.Socket.ByteString.IOVec
import Network.Socket.ByteString.Internal
import Prelude hiding (getContents)
import System.IO.Unsafe (unsafeInterleaveIO)

-- -----------------------------------------------------------------------------
-- Sending

-- | Send a message on a socket. The socket must be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for ensuring
-- that all data has been sent.
--
-- Because a lazily generated 'ByteString' may be arbitrarily long, this
-- function caps the amount it will attempt to send at 4MB. This number is
-- large (so it should not penalize performance on fast networks), but not
-- outrageously so (to avoid demanding lazily computed data unnecessarily
-- early).
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int64    -- ^ Number of bytes sent
send (MkSocket fd _ _ _ _) s = do
  let cs  = L.toChunks s
      len = length cs
  liftM fromIntegral . allocaArray len $ \ptr ->
    withPokes cs ptr $ \niovs ->
      throwSocketErrorIfMinus1RetryMayBlock "writev"
        (threadWaitWrite (fromIntegral fd)) $
        c_writev (fromIntegral fd) ptr niovs
  where
    withPokes ss p f = loop ss p 0 0
      where loop (c:cs) q k !niovs
                | k < sendLimit =
                    unsafeUseAsCStringLen c $ \(ptr,len) -> do
                      poke q $ IOVec ptr (fromIntegral len)
                      loop cs (q `plusPtr` sizeOf (undefined :: IOVec))
                              (k + fromIntegral len) (niovs + 1)
                | otherwise = f niovs
            loop _ _ _ niovs = f niovs
    -- maximum number of bytes to transmit in one system call
    sendLimit = 4194304 :: Int

-- | Send a message on a socket. The socket must be in a connected state. This
-- function continues to send data until either all data has been sent or an
-- error occurs. If there is an error, an exception is raised, and there is
-- no way to determine how much data was sent.
sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
  sent <- send sock bs
  when (sent < L.length bs) $ sendAll sock (L.drop sent bs)

-- -----------------------------------------------------------------------------
-- Receiving

-- | Receive a message from a socket. The socket must be in a connected state.
-- Data is received on demand, in chunks; each chunk will be sized to reflect
-- the amount of data received by individual 'recv_' calls.
--
-- All remaining data from the socket is consumed. When there is no more data
-- to be received, the receiving side of the socket is shut down. If there is
-- an error and an exception is thrown, the socket is not shut down.
getContents :: Socket         -- ^ Connected socket
            -> IO ByteString  -- ^ Data received
getContents sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock defaultChunkSize
    if S.null s
      then shutdown sock ShutdownReceive >> return Empty
      else Chunk s `liftM` loop

-- | Receive a message from a socket. The socket must be in a connected state.
-- This function may return fewer bytes than specified. If the message is
-- longer than the specified length, it may be discarded depending on the type
-- of socket. This function may block until a message arrives.
--
-- If there is no more data to be received, returns an empty 'ByteString'.
recv :: Socket         -- ^ Connected socket
     -> Int64          -- ^ Maximum number of bytes to receive
     -> IO ByteString  -- ^ Data received
recv sock nbytes = chunk `liftM` N.recv sock (fromIntegral nbytes) where
  chunk k
    | S.null k  = Empty
    | otherwise = Chunk k Empty
