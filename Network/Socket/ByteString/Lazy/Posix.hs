{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Socket.ByteString.Lazy.Posix
    (
    -- * Send data to a socket
      send
    , sendAll
    ) where

import Control.Monad (liftM)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))

import Network.Socket.ByteString.IOVec (IOVec(IOVec))
import Network.Socket.ByteString.Internal (c_writev)
import Network.Socket.Internal
import Network.Socket.Types

-- -----------------------------------------------------------------------------
-- Sending

send :: NetworkSocket s => s     -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int64    -- ^ Number of bytes sent
send s lbs = do
  let cs  = take maxNumChunks (L.toChunks lbs)
      len = length cs
  liftM fromIntegral . allocaArray len $ \ptr ->
    withPokes cs ptr $ \niovs ->
      throwSocketErrorWaitWrite (socketFd s) "writev" $
        c_writev (fromIntegral $ socketFd s) ptr niovs
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

sendAll :: NetworkSocket s => s     -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll s bs = do
  sent <- send s bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll s bs'
