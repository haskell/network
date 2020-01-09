{-# OPTIONS_GHC -funbox-strict-fields #-}

#include "HsNet.h"

module Network.Socket.Posix.Cmsg (
    Cmsg(..)
  , withCmsgs
  , parseCmsgs
  ) where

#include <sys/types.h>
#include <sys/socket.h>

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.ForeignPtr
import qualified Data.ByteString as B
import Data.ByteString.Internal

import Network.Socket.Imports
import Network.Socket.Posix.MsgHdr
import Network.Socket.Types

-- | Control message including a pair of level and type.
data Cmsg = Cmsg {
    cmsgLevelType :: (CInt,CInt)
  , cmsgBody      :: ByteString
  } deriving (Eq, Show)

data CmsgHdr = CmsgHdr {
    cmsgHdrLen   :: !CInt
  , cmsgHdrLevel :: !CInt
  , cmsgHdrType  :: !CInt
  } deriving (Eq, Show)

instance Storable CmsgHdr where
  sizeOf _    = (#size struct cmsghdr)
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    len <- (#peek struct cmsghdr, cmsg_len)   p
    lvl <- (#peek struct cmsghdr, cmsg_level) p
    typ <- (#peek struct cmsghdr, cmsg_type)  p
    return $ CmsgHdr len lvl typ

  poke p (CmsgHdr len lvl typ) = do
    zeroMemory p (#size struct cmsghdr)
    (#poke struct cmsghdr, cmsg_len)   p len
    (#poke struct cmsghdr, cmsg_level) p lvl
    (#poke struct cmsghdr, cmsg_type)  p typ

withCmsgs :: [Cmsg] -> (Ptr CmsgHdr -> Int -> IO a) -> IO a
withCmsgs cmsgs0 action
  | total == 0 = action nullPtr 0
  | otherwise  = allocaBytes total $ \ctrlPtr -> do
        loop ctrlPtr cmsgs0 spaces
        action ctrlPtr total
  where
    loop ctrlPtr (cmsg:cmsgs) (s:ss) = do
        encodeCmsg ctrlPtr cmsg
        let nextPtr = ctrlPtr `plusPtr` s
        loop nextPtr cmsgs ss
    loop _ _ _ = return ()
    cmsg_space = fromIntegral . c_cmsg_space . fromIntegral
    spaces = map (cmsg_space . B.length . cmsgBody) cmsgs0
    total = sum spaces

encodeCmsg :: Ptr CmsgHdr -> Cmsg -> IO ()
encodeCmsg ctrlPtr (Cmsg (lvl,typ) (PS fptr off len)) = do
    poke ctrlPtr $ CmsgHdr (c_cmsg_len (fromIntegral len)) lvl typ
    withForeignPtr fptr $ \src0 -> do
        let src = src0 `plusPtr` off
        dst <- c_cmsg_data ctrlPtr
        memcpy dst src len

parseCmsgs :: Ptr MsgHdr -> IO [Cmsg]
parseCmsgs msgptr = do
    ptr <- c_cmsg_firsthdr msgptr
    loop ptr id
  where
    loop ptr build
      | ptr == nullPtr = return $ build []
      | otherwise = do
            cmsg <- decodeCmsg ptr
            nextPtr <- c_cmsg_nxthdr msgptr ptr
            loop nextPtr (build . (cmsg :))

decodeCmsg :: Ptr CmsgHdr -> IO Cmsg
decodeCmsg ptr = do
    CmsgHdr len lvl typ <- peek ptr
    src <- c_cmsg_data ptr
    let siz = fromIntegral len - (src `minusPtr` ptr)
    Cmsg (lvl,typ) <$> create (fromIntegral siz) (\dst -> memcpy dst src siz)

foreign import ccall unsafe "cmsg_firsthdr"
  c_cmsg_firsthdr :: Ptr MsgHdr -> IO (Ptr CmsgHdr)

foreign import ccall unsafe "cmsg_nxthdr"
  c_cmsg_nxthdr :: Ptr MsgHdr -> Ptr CmsgHdr -> IO (Ptr CmsgHdr)

foreign import ccall unsafe "cmsg_data"
  c_cmsg_data :: Ptr CmsgHdr -> IO (Ptr Word8)

foreign import ccall unsafe "cmsg_space"
  c_cmsg_space :: CInt -> CInt

foreign import ccall unsafe "cmsg_len"
  c_cmsg_len :: CInt -> CInt
