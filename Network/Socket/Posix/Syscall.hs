{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-- | The POSIX parts of "Network.Socket.Syscall".
module Network.Socket.Posix.Syscall (
    setupSocket,
    setupAccepted,
    acceptFd,
    acceptFdNoWait,
    reportConnReset,
    ConnectNext (..),
    connectNext,
) where

import Data.Functor.Identity (Identity (..))
import Data.Traversable (for)
import Foreign.C.Error (eAGAIN, eINPROGRESS, eINTR, eWOULDBLOCK, getErrno)
import Foreign.Marshal.Utils (with)

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
import Network.Socket.Cbits
#else
import Network.Socket.Fcntl
#endif
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

-- | Preparing a new socket: non-blocking, unless 'socket' asked for it.
setupSocket :: CSocket -> IO ()
#ifdef HAVE_ADVANCED_SOCKET_FLAGS
setupSocket _ = return ()
#else
setupSocket = setNonBlockIfNeeded
#endif

-- | Preparing an accepted socket: 'acceptFd' has done it.
setupAccepted :: CSocket -> IO ()
setupAccepted _ = return ()

-- | Accepting a connection on a non-blocking listening socket, with the
--   length of the peer's address.
acceptFd :: Socket -> CSocket -> Ptr sa -> Int -> IO (CSocket, Int)
acceptFd listing_sock fd sa sz = runIdentity <$>
    acceptFdBy (\loc io -> Identity <$> throwSocketErrorWaitRead listing_sock loc io) fd sa sz

-- | 'acceptFd' which does not wait: 'Nothing' if no connection is queued.
acceptFdNoWait :: CSocket -> Ptr sa -> Int -> IO (Maybe (CSocket, Int))
acceptFdNoWait = acceptFdBy $ \loc io ->
    let loop = do
            r <- io
            if r /= -1 then return (Just r) else do
                err <- getErrno
                if err == eINTR then loop
                else if err == eAGAIN || err == eWOULDBLOCK then return Nothing
                else throwSocketError loc
     in loop

acceptFdBy
    :: Traversable f
    => (String -> IO CSocket -> IO (f CSocket))
    -> CSocket -> Ptr sa -> Int -> IO (f (CSocket, Int))
acceptFdBy call fd sa sz = with (fromIntegral sz) $ \ptr_len -> do
#ifdef HAVE_ADVANCED_SOCKET_FLAGS
    r <- call "Network.Socket.accept"
        (c_accept4 fd sa ptr_len (sockNonBlock .|. sockCloexec))
#else
    r <- call "Network.Socket.accept" (c_accept fd sa ptr_len)
#endif
    for r $ \new_fd -> do
#ifndef HAVE_ADVANCED_SOCKET_FLAGS
        setNonBlockIfNeeded new_fd
        setCloseOnExecIfNeeded new_fd
#endif
        len <- peek ptr_len
        return (new_fd, fromIntegral len)

-- | POSIX says nothing of an ICMP unreachable to an unconnected socket.
reportConnReset :: Socket -> Bool -> IO ()
reportConnReset _ _ = return ()

-- | What to do after @connect@ has failed.
data ConnectNext = ConnectRetry | ConnectWait

-- | Retrying an interrupted @connect@, waiting for one in progress, or
--   throwing.
connectNext :: String -> IO ConnectNext
connectNext errLoc = do
    err <- getErrno
    if err == eINTR
        then return ConnectRetry
        else if err == eINPROGRESS
            then return ConnectWait
            else throwSocketError errLoc

#ifdef HAVE_ADVANCED_SOCKET_FLAGS
foreign import CALLCONV unsafe "accept4"
  c_accept4 :: CSocket -> Ptr sa -> Ptr CInt{-CSockLen???-} -> CInt -> IO CSocket
#else
foreign import CALLCONV unsafe "accept"
  c_accept :: CSocket -> Ptr sa -> Ptr CInt{-CSockLen???-} -> IO CSocket
#endif
