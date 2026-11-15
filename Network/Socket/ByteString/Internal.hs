-- |
-- Module      : Network.Socket.ByteString.Internal
-- Copyright   : (c) Johan Tibell 2007-2010
-- License     : BSD-style
--
-- Maintainer  : johan.tibell@gmail.com
-- Stability   : stable
-- Portability : portable
--
module Network.Socket.ByteString.Internal
    (
      mkInvalidRecvArgError
    , withBufSizs
    ) where

import Data.ByteString.Internal (ByteString (..))
import Foreign.ForeignPtr (withForeignPtr)
import GHC.IO.Exception (IOErrorType(..))
import System.IO.Error (ioeSetErrorString, mkIOError)

import Network.Socket.Imports

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

-- | The buffers of the parts, with their lengths.
withBufSizs :: [ByteString] -> ([(Ptr Word8, Int)] -> IO a) -> IO a
withBufSizs bss0 f = loop bss0 id
  where
    loop []                    build = f $ build []
    loop (PS fptr off len:bss) build = withForeignPtr fptr $ \ptr -> do
        let ptr' = ptr `plusPtr` off
        loop bss (build . ((ptr',len) :))
