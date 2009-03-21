{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Socket.ByteString.Internal
  ( mkInvalidRecvArgError
  , c_writev
  , c_sendmsg
  ) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Network.Socket.ByteString.IOVec (IOVec)
import Network.Socket.ByteString.MsgHdr (MsgHdr)
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.Posix.Types (CSsize)

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase (IOErrorType(..))
#elif __HUGS__
import Hugs.Prelude (IOErrorType(..))
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
#ifdef __GLASGOW_HASKELL__
                                    InvalidArgument
#else
                                    IllegalOperation
#endif
                                    loc Nothing Nothing) "non-positive length"

foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize
