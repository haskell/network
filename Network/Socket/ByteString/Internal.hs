{-# LANGUAGE CPP, ForeignFunctionInterface #-}

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
#if !defined(mingw32_HOST_OS)
    , c_writev
    , c_sendmsg
#endif
    ) where

import System.IO.Error (ioeSetErrorString, mkIOError)

#if !defined(mingw32_HOST_OS)
# if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(..))
import System.Posix.Types (CSsize(..))
# else
import Foreign.C.Types (CInt)
import System.Posix.Types (CSsize)
# endif
import Foreign.Ptr (Ptr)

import Network.Socket.ByteString.IOVec (IOVec)
import Network.Socket.ByteString.MsgHdr (MsgHdr)
#endif

#if __GLASGOW_HASKELL__ < 611
import GHC.IOBase (IOErrorType(..))
#else
import GHC.IO.Exception (IOErrorType(..))
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize
#endif
