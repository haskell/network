{-# LANGUAGE CPP #-}

-- | Lazily loading Winsock extension functions.
--
-- Winsock does not export @WSASendMsg@ and @WSARecvMsg@ as ordinary
-- symbols.  They have to be looked up at run time with @WSAIoctl@ and
-- @SIO_GET_EXTENSION_FUNCTION_POINTER@, which needs a live socket.  The
-- resulting pointer is process-wide, so it is fetched once and cached.
--
-- Doing the lookup here rather than in C is what lets the WinIO path
-- issue the call itself and hand it to @withOverlapped@.
module Network.Socket.Win32.Load (
    -- * Generic loader
    Loaded(..)
  , loadExtensionFunction
    -- * WSASendMsg and WSARecvMsg
  , WSASendMsgFn
  , WSARecvMsgFn
  , getWSASendMsg
  , getWSARecvMsg
  , mkSendMsgUnsafe
  , mkRecvMsgUnsafe
  ) where

#include "HsNetDef.h"

import Control.Concurrent.STM
import Control.Exception (onException)
import System.IO.Unsafe (unsafePerformIO)

import Network.Socket.Imports
import Network.Socket.Internal (throwSocketError)
import Network.Socket.Types (CSocket)

type DWORD   = Word32
type LPDWORD = Ptr DWORD

-- | Cache state of a lazily loaded extension function.
data Loaded a = Unloaded | Loading | Loaded a

-- | Look an extension function up once and cache it.  A concurrent
-- caller waits for the in-flight lookup instead of issuing a redundant
-- @WSAIoctl@.  On failure the cache is reset so that a later call, which
-- may have a usable socket, can try again.
loadExtensionFunction
    :: TVar (Loaded (FunPtr a))
    -- ^ Cache shared by all sockets.
    -> (CSocket -> IO (FunPtr a))
    -- ^ The C side loader, returning a null pointer on failure.
    -> String
    -- ^ Function name, for the error message.
    -> CSocket
    -> IO (FunPtr a)
loadExtensionFunction var load fname s = do
    mfp <- atomically $ do
        st <- readTVar var
        case st of
            Unloaded  -> do
                writeTVar var Loading
                return Nothing
            Loading   -> retry
            Loaded fp -> return $ Just fp
    case mfp of
        Just fp -> return fp
        Nothing -> load' `onException` atomically (writeTVar var Unloaded)
  where
    load' = do
        fp <- load s
        when (fp == nullFunPtr) $
            throwSocketError $ "Network.Socket: cannot load " ++ fname
        atomically $ writeTVar var $ Loaded fp
        return fp

-- The message header is kept as @Ptr ()@ so that one cache serves every
-- socket address type; callers cast it.
type WSASendMsgFn = CSocket -> Ptr () -> DWORD -> LPDWORD -> Ptr () -> Ptr () -> IO CInt
type WSARecvMsgFn = CSocket -> Ptr () -> LPDWORD -> Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "loadWSASendMsg"
  c_loadWSASendMsg :: CSocket -> IO (FunPtr WSASendMsgFn)
foreign import ccall unsafe "loadWSARecvMsg"
  c_loadWSARecvMsg :: CSocket -> IO (FunPtr WSARecvMsgFn)


-- | The calls return at once: overlapped, or on a non-blocking socket.
foreign import CALLCONV unsafe "dynamic"
  mkSendMsgUnsafe :: FunPtr WSASendMsgFn -> WSASendMsgFn
foreign import CALLCONV unsafe "dynamic"
  mkRecvMsgUnsafe :: FunPtr WSARecvMsgFn -> WSARecvMsgFn

sendMsgCache :: TVar (Loaded (FunPtr WSASendMsgFn))
sendMsgCache = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE sendMsgCache #-}

recvMsgCache :: TVar (Loaded (FunPtr WSARecvMsgFn))
recvMsgCache = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE recvMsgCache #-}

getWSASendMsg :: CSocket -> IO (FunPtr WSASendMsgFn)
getWSASendMsg = loadExtensionFunction sendMsgCache c_loadWSASendMsg "WSASendMsg"

getWSARecvMsg :: CSocket -> IO (FunPtr WSARecvMsgFn)
getWSARecvMsg = loadExtensionFunction recvMsgCache c_loadWSARecvMsg "WSARecvMsg"
