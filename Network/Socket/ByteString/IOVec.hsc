-- | Support module for the POSIX writev system call.
module Network.Socket.ByteString.IOVec
  ( IOVec(..)
  , sizeOfIOVec
  ) where

import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

#include <sys/uio.h>

data IOVec = IOVec
    { iovBase :: Ptr CChar
    , iovLen  :: CSize
    }

sizeOfIOVec :: Int
sizeOfIOVec = (#const sizeof(struct iovec))

instance Storable IOVec where
  sizeOf _    = sizeOfIOVec
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    base <- (#peek struct iovec, iov_base) p
    len  <- (#peek struct iovec, iov_len)  p
    return $ IOVec base len

  poke p iov = do
    (#poke struct iovec, iov_base) p (iovBase iov)
    (#poke struct iovec, iov_len)  p (iovLen  iov)
