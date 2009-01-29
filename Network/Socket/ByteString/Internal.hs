{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Socket.ByteString.Internal
    (
      mkInvalidRecvArgError
    , mkEOFError
    , throwErrnoIfMinus1Retry_mayBlock
    , throwErrnoIfMinus1Retry_repeatOnBlock
    , c_writev
    , c_sendmsg
    ) where

import Foreign.C.Error (eAGAIN, eINTR, eWOULDBLOCK, getErrno, throwErrno)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import GHC.IOBase (IOErrorType(..), IOException(..))
import Network.Socket.ByteString.IOVec (IOVec)
import Network.Socket.ByteString.MsgHdr (MsgHdr)
import Prelude hiding (repeat)
import System.Posix.Types (CSsize)

-----------------------------------------------------------------------------
-- Support for thread-safe blocking operations in GHC.

#if defined(__GLASGOW_HASKELL__) && !(defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS))
{-# SPECIALISE
    throwErrnoIfMinus1Retry_mayBlock
         :: String -> IO CInt -> IO CInt -> IO CInt #-}
throwErrnoIfMinus1Retry_mayBlock :: Num a => String -> IO a -> IO a -> IO a
throwErrnoIfMinus1Retry_mayBlock name on_block act = do
    res <- act
    if res == -1
        then do
            err <- getErrno
            if err == eINTR
                then throwErrnoIfMinus1Retry_mayBlock name on_block act
                else if err == eWOULDBLOCK || err == eAGAIN
                        then on_block
                        else throwErrno name
        else return res

throwErrnoIfMinus1Retry_repeatOnBlock :: Num a => String -> IO b -> IO a -> IO a
throwErrnoIfMinus1Retry_repeatOnBlock name on_block act = do
  throwErrnoIfMinus1Retry_mayBlock name (on_block >> repeat) act
  where repeat = throwErrnoIfMinus1Retry_repeatOnBlock name on_block act

#else
throwErrnoIfMinus1Retry_mayBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

throwErrnoIfMinus1Retry_repeatOnBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

# if defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS)
throwSocketErrorIfMinus1Retry name act = do
  r <- act
  if (r == -1)
   then do
    rc   <- c_getLastError
    case rc of
      10093 -> do -- WSANOTINITIALISED
        withSocketsDo (return ())
        r <- act
        if (r == -1)
         then (c_getLastError >>= throwSocketError name)
         else return r
      _ -> throwSocketError name rc
   else return r

#ifndef CALLCONV
# ifdef WITH_WINSOCK
#  define CALLCONV stdcall
# else
#  define CALLCONV ccall
# endif
#endif

foreign import CALLCONV unsafe "WSAGetLastError"
  c_getLastError :: IO CInt

# else
throwSocketErrorIfMinus1Retry name act = throwErrnoIfMinus1Retry name act
# endif
#endif /* __GLASGOW_HASKELL */

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = IOError Nothing
#ifdef __GLASGOW_HASKELL__
                                    InvalidArgument
#else
                                    IllegalOperation
#endif
                                    loc "non-positive length" Nothing

mkEOFError :: String -> IOError
mkEOFError loc = IOError Nothing EOF loc "end of file" Nothing

foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import ccall unsafe "sendmsg"
  c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CSsize
