{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Types.Posix (
    -- * Socket type
      Socket
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
import Foreign.C.Error (throwErrno)
import GHC.Conc (closeFdWith)
import System.Posix.Types (Fd)
import GHC.Exts (touch##)
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.IO (IO (..))

import Network.Socket.Imports

-----------------------------------------------------------------------------

-- | Basic type for a socket.
data Socket = Socket (IORef CInt) CInt {- for Show -}

instance Show Socket where
    show (Socket _ ofd) = "<socket: " ++ show ofd ++ ">"

instance Eq Socket where
    Socket ref1 _ == Socket ref2 _ = ref1 == ref2

unsafeFdSocket :: Socket -> IO CInt
unsafeFdSocket (Socket ref _) = readIORef ref

touchSocket :: Socket -> IO ()
touchSocket (Socket ref _) = touch ref

touch :: IORef a -> IO ()
touch (IORef (STRef mutVar)) =
  -- Thanks to a GHC issue, this touch# may not be quite guaranteed
  -- to work. There's talk of replacing the touch# primop with one
  -- that works better with the optimizer. But this seems to be the
  -- "right" way to do it for now.
  IO $ \s -> (## touch## mutVar s, () ##)


withFdSocket :: Socket -> (CInt -> IO r) -> IO r
withFdSocket (Socket ref _) f = do
  fd <- readIORef ref
  -- Should we throw an exception if the socket is already invalid?
  -- That will catch some mistakes but certainly not all.

  r <- f fd

  touch ref
  return r

socketToFd :: Socket -> IO CInt
socketToFd s = do
#if defined(mingw32_HOST_OS)
    fd <- unsafeFdSocket s
    fd2 <- c_wsaDuplicate fd
    -- FIXME: throw error no if -1
    close s
    return fd2

foreign import ccall unsafe "wsaDuplicate"
   c_wsaDuplicate :: CInt -> IO CInt
#else
    fd <- unsafeFdSocket s
    -- FIXME: throw error no if -1
    fd2 <- c_dup fd
    close s
    return fd2

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt
#endif

mkSocket :: CInt -> IO Socket
mkSocket fd = do
    ref <- newIORef fd
    let s = Socket ref fd
    void $ mkWeakIORef ref $ close s
    return s

invalidSocket :: CInt
#if defined(mingw32_HOST_OS)
invalidSocket = #const INVALID_SOCKET
#else
invalidSocket = -1
#endif

invalidateSocket ::
      Socket
   -> (CInt -> IO a)
   -> (CInt -> IO a)
   -> IO a
invalidateSocket (Socket ref _) errorAction normalAction = do
    oldfd <- atomicModifyIORef' ref $ \cur -> (invalidSocket, cur)
    if oldfd == invalidSocket then errorAction oldfd else normalAction oldfd

close :: Socket -> IO ()
close s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    -- closeFd ignores the return value of c_close and
    -- does not throw exceptions
    closeFd :: Fd -> IO ()
    closeFd = void . c_close . fromIntegral

close' :: Socket -> IO ()
close' s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    closeFd :: Fd -> IO ()
    closeFd fd = do
        ret <- c_close $ fromIntegral fd
        when (ret == -1) $ throwErrno "Network.Socket.close'"

#if defined(mingw32_HOST_OS)
foreign import CALLCONV unsafe "closesocket"
  c_close :: CInt -> IO CInt
#else
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt
#endif
