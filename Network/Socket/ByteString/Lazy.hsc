{-# LANGUAGE CPP, ForeignFunctionInterface #-}

#include <sys/uio.h>

module Network.Socket.ByteString.Lazy
    (
      getContents
    , send
    , recv
    ) where

import Control.Monad (liftM)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString.Lazy.Internal (ByteString(..), defaultChunkSize)
import Data.Int (Int64)
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import qualified Network.Socket.ByteString as N
import Network.Socket.ByteString.Internal
import Network.Socket (Socket(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Prelude hiding (getContents)
import System.Posix.Types (CSsize)
import GHC.Conc (threadWaitRead, threadWaitWrite)

data IOVec = IOVec { iovBase :: Ptr CChar
                   , iovLen :: CSize }

instance Storable IOVec where
    sizeOf _    = (#const sizeof(struct iovec))
    alignment _ = alignment (undefined :: CInt)

    peek p = do
      base <- (#peek struct iovec, iov_base) p
      len <- (#peek struct iovec, iov_len) p
      return (IOVec base len)

    poke p iov = do
      (#poke struct iovec, iov_base) p (iovBase iov)
      (#poke struct iovec, iov_len) p (iovLen iov)

send :: Socket -> ByteString -> IO Int64
send (MkSocket fd _ _ _ _) s = do
    let cs = L.toChunks s
        len = length cs
    liftM fromIntegral . allocaArray len $ \ptr ->
      withPokes cs ptr $
        throwErrnoIfMinus1Retry_repeatOnBlock "writev"
          (threadWaitWrite (fromIntegral fd)) $
          c_writev (fromIntegral fd) ptr (fromIntegral len)
  where
    withPokes ss p f = loop ss p
      where loop (s:ss) q =
              unsafeUseAsCStringLen s $ \(ptr,len) -> do
                let iov = IOVec ptr (fromIntegral len)
                poke q iov
                loop ss (q `plusPtr` sizeOf iov)
            loop _ _ = f

foreign import ccall unsafe "send"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

getContents :: Socket -> IO ByteString    
getContents sock = do
  s <- N.recv sock defaultChunkSize
  if S.null s
    then return Empty
    else Chunk s `liftM` unsafeInterleaveIO (getContents sock)

recv :: Socket -> Int64 -> IO ByteString
recv sock nbytes = chunk `liftM` N.recv sock (fromIntegral nbytes)
    where chunk k = Chunk k Empty
