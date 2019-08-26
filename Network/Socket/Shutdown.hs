{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#include "HsNetDef.h"

module Network.Socket.Shutdown (
    ShutdownCmd(..)
  , shutdown
  , gracefulClose
  ) where

import Control.Concurrent (putMVar, takeMVar, newEmptyMVar)
import qualified Control.Exception as E
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified GHC.Event as Ev
import System.Posix.Types (Fd(..))

import Network.Socket.Buffer
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

data ShutdownCmd = ShutdownReceive
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
shutdown s stype = void $ withFdSocket s $ \fd ->
  throwSocketErrorIfMinus1Retry_ "Network.Socket.shutdown" $
    c_shutdown fd $ sdownCmdToInt stype

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

data Wait = MoreData | TimeoutTripped

-- | Closing a socket gracefully.
--   This sends TCP FIN and check if TCP FIN is received from the peer.
--   The second argument is time out to receive TCP FIN in millisecond.
--   In both normal cases and error cases, socket is deallocated finally.
--
--   Since: 3.1.1.0
gracefulClose :: Socket -> Int -> IO ()
gracefulClose s tmout = (sendRecvFIN `E.finally` close s) `E.catch` ignore
  where
    sendRecvFIN = do
        -- Sending TCP FIN.
        shutdown s ShutdownSend
        -- Waiting TCP FIN.
        recvEOF
    recvEOF = do
        Just evmgr <- Ev.getSystemEventManager
        tmmgr <- Ev.getSystemTimerManager
        mvar <- newEmptyMVar
        E.bracket (register evmgr tmmgr mvar) (unregister evmgr tmmgr) $ \_ -> do
            wait <- takeMVar mvar
            case wait of
              TimeoutTripped -> return ()
              -- We don't check the (positive) length.
              -- In normal case, it's 0. That is, only FIN is received.
              -- In error cases, data is available. But there is no
              -- application which can read it. So, let's stop receiving
              -- to prevent attacks.
              MoreData       -> E.bracket (mallocBytes bufSize)
                                          free
                                          (\buf -> void $ recvBufNoWait s buf bufSize)
    register evmgr tmmgr mvar = do
        -- millisecond to microsecond
        key1 <- Ev.registerTimeout tmmgr (tmout * 1000) $
            putMVar mvar TimeoutTripped
        key2 <- withFdSocket s $ \fd' -> do
            let callback _ _ = putMVar mvar MoreData
                fd = Fd fd'
            Ev.registerFd evmgr callback fd Ev.evtRead Ev.OneShot
        return (key1, key2)
    unregister evmgr tmmgr (key1,key2) = do
        Ev.unregisterTimeout tmmgr key1
        Ev.unregisterFd evmgr key2
    -- Don't use 4092 here. The GHC runtime takes the global lock
    -- if the length is over 3276 bytes in 32bit or 3272 bytes in 64bit.
    bufSize = 1024
    -- shutdown sometime returns ENOTCONN.
    -- Probably, we don't want to log this error.
    ignore :: E.IOException -> IO ()
    ignore _e = return ()
