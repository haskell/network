{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#include "HsNetDef.h"

module Network.Socket.Close (
    ShutdownCmd(..)
  , shutdown
  , close
  ) where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Typeable
import Foreign.C.Types (CInt(..))
import GHC.Conc (closeFdWith)

import Network.Socket.Internal
import Network.Socket.Types

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
shutdown Socket{..} stype = do
  throwSocketErrorIfMinus1Retry_ "Network.Socket.shutdown" $
    c_shutdown socketFd (sdownCmdToInt stype)
  return ()

-- -----------------------------------------------------------------------------

-- | Close the socket. Sending data to or receiving data from closed socket
-- may lead to undefined behaviour.
close :: Socket -> IO ()
close Socket{..} =
 modifyMVar_ socketStatus $ \status ->
   case status of
     -- This is called by the finalizer of MVar if 'socketToHandle'
     -- is used. The finalizer catches and ignores this exception.
     ConvertedToHandle ->
         ioError (userError "close: converted to a Handle, use hClose instead")
     Closed ->
         return status
     _ -> closeFdWith (closeFd . fromIntegral) (fromIntegral socketFd) >> return Closed

closeFd :: CInt -> IO ()
closeFd fd = throwSocketErrorIfMinus1_ "Network.Socket.close" $ c_close fd

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

#if defined(mingw32_HOST_OS)
foreign import CALLCONV unsafe "closesocket"
  c_close :: CInt -> IO CInt
#else
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt
#endif
