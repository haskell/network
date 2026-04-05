module Network.Socket.Win32.Load (
    Loaded(..),
    loadExtensionFunction,
) where

import Control.Concurrent.STM
import GHC.Windows
import Network.Socket.Imports
import Network.Socket.Internal (throwSocketError)
import Network.Socket.Types.WinIO (SOCKET)

data Loaded a = Unloaded | Loading | Loaded a

-- | Load a Winsock extension function once, caching the result in a TVar.
-- The C loader uses WSAIoctl with SIO_GET_EXTENSION_FUNCTION_POINTER.
loadExtensionFunction
    :: TVar (Loaded a)
    -> (SOCKET -> IO (FunPtr a))
    -> (FunPtr a -> a)
    -> String
    -> SOCKET -> IO a
loadExtensionFunction var load mk fname s = do
    mfn <- atomically $ do
        c <- readTVar var
        case c of
            Unloaded -> do
                writeTVar var Loading
                pure Nothing
            Loading -> retry
            Loaded l -> pure $ Just l
    case mfn of
        Nothing -> do
            fp <- load s
            when (fp == nullFunPtr) $
                throwSocketError ("Network.Socket: can't load " <> fname)
            let fn = mk fp
            atomically $ writeTVar var $ Loaded fn
            pure fn
        Just fn -> pure fn
