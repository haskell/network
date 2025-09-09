{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

##include "HsNetDef.h"
#include "HsNet.h"

module Network.Socket.Types.WinIO (
    -- * Socket type
      Socket
    , SOCKET
    , invalidSocket
    , withSOCKET
    , withFdSocket
    , unsafeFdSocket
    , touchSocket
    , socketToFd
    , mkSocket
    , invalidateSocket
    , close
    , close'
    , c_close
    ) where

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', mkWeakIORef)
import GHC.Exts (touch##)
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.IO (IO (..))
import GHC.Windows

import Network.Socket.Imports

-----------------------------------------------------------------------------
type SOCKET = HANDLE

-- | Basic type for a socket.
data Socket = Socket (IORef SOCKET) SOCKET {- for Show -}

instance Show Socket where
    show (Socket _ ofd) = "<socket: " ++ show ofd ++ ">"

instance Eq Socket where
    Socket ref1 _ == Socket ref2 _ = ref1 == ref2

-- ew
winsockToInt :: SOCKET -> CInt
winsockToInt = fromIntegral . ptrToIntPtr

unsafeSOCKET :: Socket -> IO SOCKET
unsafeSOCKET (Socket ref _) = readIORef ref

unsafeFdSocket :: Socket -> IO CInt
unsafeFdSocket s = winsockToInt <$> unsafeSOCKET s

touchSocket :: Socket -> IO ()
touchSocket (Socket ref _) = touch ref

touch :: IORef a -> IO ()
touch (IORef (STRef mutVar)) =
  -- See Types.Posix
  IO $ \s -> (## touch## mutVar s, () ##)

withFdSocket :: Socket -> (CInt -> IO r) -> IO r
withFdSocket (Socket ref _) f = do
  s <- readIORef ref
  -- Should we throw an exception if the socket is already invalid?
  -- That will catch some mistakes but certainly not all.

  r <- f $ winsockToInt s

  touch ref
  return r

withSOCKET :: Socket -> (SOCKET -> IO r) -> IO r
withSOCKET (Socket ref _ ) f = do
    s <- readIORef ref
    r <- f s
    touch ref
    return r

socketToFd :: Socket -> IO CInt
socketToFd s = do
    sock <- unsafeSOCKET s
    sock2 <- c_wsaDuplicate sock
    -- FIXME: throw error no if -1
    close s
    return $ winsockToInt sock2

foreign import ccall unsafe "wsaDuplicate"
   c_wsaDuplicate :: SOCKET -> IO SOCKET

mkSocket :: SOCKET -> IO Socket
mkSocket sock = do
    ref <- newIORef sock
    let s = Socket ref sock
    void $ mkWeakIORef ref $ close s
    return s

invalidSocket :: SOCKET
invalidSocket = intPtrToPtr $ #const INVALID_SOCKET
{-# INLINE invalidSocket #-}

invalidateSocket ::
      Socket
   -> (SOCKET -> IO a)
   -> (SOCKET -> IO a)
   -> IO a
invalidateSocket (Socket ref _) errorAction normalAction = do
    oldsock <- atomicModifyIORef' ref $ \cur -> (invalidSocket, cur)
    if oldsock == invalidSocket then errorAction oldsock else normalAction oldsock

close :: Socket -> IO ()
close s = invalidateSocket s (\_ -> return ()) $ \oldsock ->
    -- closeFdWith avoids the deadlock of IO manager.
    void $ c_close oldsock

close' :: Socket -> IO ()
close' s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- IOCP doesn't give us the same deadlock opportunities... right?
    failIf_ (/= 0) "Network.Socket.close'" $ c_close oldfd

foreign import ccall unsafe "closesocket"
  c_close :: SOCKET -> IO CInt
