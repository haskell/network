{-# LANGUAGE CPP #-}

module Network.Socket.Close (
    ShutdownCmd(..)
  , shutdown
  , close
  ) where

#include "HsNetDef.h"

import Control.Concurrent.MVar (modifyMVar_)
import Data.Typeable
import Foreign.C.Types (CInt(..))

import Network.Socket.Internal
import Network.Socket.Types

#if MIN_VERSION_base(4,3,1)
import GHC.Conc (closeFdWith)
#else
closeFdWith closer fd = closer fd
#endif

-- -----------------------------------------------------------------------------

data ShutdownCmd
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth
 deriving Typeable

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

-- | Shut down one or both halves of the connection, depending on the
-- second argument to the function.  If the second argument is
-- 'ShutdownReceive', further receives are disallowed.  If it is
-- 'ShutdownSend', further sends are disallowed.  If it is
-- 'ShutdownBoth', further sends and receives are disallowed.
shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry_ "Network.Socket.shutdown" $
    c_shutdown s (sdownCmdToInt stype)
  return ()

-- -----------------------------------------------------------------------------

-- | Close the socket. Sending data to or receiving data from closed socket
-- may lead to undefined behaviour.
close :: Socket -> IO ()
close (MkSocket s _ _ _ sockStatus) = do
 modifyMVar_ sockStatus $ \ status ->
   case status of
     ConvertedToHandle ->
         ioError (userError ("close: converted to a Handle, use hClose instead"))
     Closed ->
         return status
     _ -> closeFdWith (closeFd . fromIntegral) (fromIntegral s) >> return Closed

closeFd :: CInt -> IO ()
closeFd fd = throwSocketErrorIfMinus1_ "Network.Socket.close" $ c_close fd

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

#if defined(WITH_WINSOCK)
foreign import CALLCONV unsafe "closesocket"
  c_close :: CInt -> IO CInt
#else
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt
#endif
