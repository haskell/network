#include <sys/uio.h>

-- | Support module for the POSIX writev system call.
module Network.Socket.ByteString.IOVec where

import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

data IOVec = IOVec
    { iovBase :: Ptr CChar
    , iovLen  :: CSize
    }

sizeOfIOVec :: Int
sizeOfIOVec = sizeOf $ IOVec nullPtr 0

instance Storable IOVec where
  sizeOf _    = (#const sizeof(struct iovec))
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    base <- (#peek struct iovec, iov_base) p
    len  <- (#peek struct iovec, iov_len)  p
    return $ IOVec base len

  poke p iov = do
    (#poke struct iovec, iov_base) p (iovBase iov)
    (#poke struct iovec, iov_len)  p (iovLen  iov)
