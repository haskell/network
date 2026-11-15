{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-- | The Windows parts of "Network.Socket.Syscall".
module Network.Socket.Win32.Syscall (
    setupSocket,
    setupAccepted,
    acceptFd,
    reportConnReset,
    ConnectNext (..),
    connectNext,
) where

import qualified GHC.Event.Windows as Mgr
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import System.Win32.Types (failIfFalse_)

import Network.Socket.Cbits
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

-- | Preparing a new socket: see 'associateSocket'.
setupSocket :: CSocket -> IO ()
setupSocket = associateSocket "Network.Socket.socket"

-- | Preparing an accepted socket: see 'associateSocket'.
-- Clears the @HANDLE_FLAG_INHERIT@ flag, which is equivalent to setting @SOCK_CLOEXEC@ on POSIX
-- to prevent child processes from inheriting the socket.
setupAccepted :: CSocket -> IO ()
setupAccepted fd = do
    failIfFalse_ "Network.Socket.accept: SetHandleInformation" $
        c_SetHandleInformation (wordPtrToPtr $ fromIntegral fd) handleFlagInherit 0
    associateSocket "Network.Socket.accept" fd

-- | Accepting a connection on a non-blocking listening socket, with the
--   length of the peer's address.
acceptFd :: Socket -> CSocket -> Ptr sa -> Int -> IO (CSocket, Int)
acceptFd listing_sock fd sa sz = with (fromIntegral sz) $ \ptr_len -> do
    new_fd <- throwSocketErrorWaitRead listing_sock "Network.Socket.accept"
        (c_accept fd sa ptr_len)
    len <- peek ptr_len
    return (new_fd, fromIntegral len)

-- | Associating a socket with the I/O manager, skipping the completion
--   port for an operation that finishes at once, and making it
--   non-blocking, as on POSIX.
--   A failure of @SetFileCompletionNotificationModes@ is a Win32 error, not
--   a Winsock one: raised with its message by 'failIfFalse_'.
associateSocket :: String -> CSocket -> IO ()
associateSocket loc fd = do
    let h = wordPtrToPtr $ fromIntegral fd
    Mgr.associateHandle' h
    -- Not FILE_SKIP_SET_EVENT_ON_HANDLE: cancelling waits on the handle.
    failIfFalse_ (loc ++ ": SetFileCompletionNotificationModes") $
        c_SetFileCompletionNotificationModes h fileSkipCompletionPortOnSuccess
    throwSocketErrorIfMinus1_ loc $ c_set_nonblock fd

-- | Whether the socket reports the ICMP unreachable an earlier send of
--   its own caused: see 'Network.Socket.Syscall.reportConnReset'.
reportConnReset :: Socket -> Bool -> IO ()
reportConnReset s report =
    with (if report then 1 else 0 :: Word32) $ \val ->
    alloca $ \ret ->
      void $ withFdSocket s $ \fd ->
        c_WSAIoctl fd sioUDPConnReset val 4 nullPtr 0 ret nullPtr nullPtr

-- | What to do after @connect@ has failed.
data ConnectNext = ConnectRetry | ConnectWait

-- | Waiting for a @connect@ in progress, or throwing.
connectNext :: String -> IO ConnectNext
connectNext errLoc = do
    rc <- c_getLastError
    if rc == wsaEWOULDBLOCK
        then return ConnectWait
        else throwSocketErrorCode errLoc rc

foreign import CALLCONV unsafe "accept"
  c_accept :: CSocket -> Ptr sa -> Ptr CInt{-CSockLen???-} -> IO CSocket

foreign import ccall unsafe "hsnet_set_nonblock"
    c_set_nonblock :: CSocket -> IO CInt

foreign import ccall unsafe "windows.h SetHandleInformation"
    c_SetHandleInformation :: Ptr () -> Word32 -> Word32 -> IO Bool

foreign import ccall unsafe "windows.h SetFileCompletionNotificationModes"
    c_SetFileCompletionNotificationModes :: Ptr () -> Word8 -> IO Bool

-- int WSAIoctl(SOCKET, DWORD, LPVOID, DWORD, LPVOID, DWORD, LPDWORD,
--              LPWSAOVERLAPPED, LPWSAOVERLAPPED_COMPLETION_ROUTINE)
foreign import CALLCONV unsafe "WSAIoctl"
  c_WSAIoctl :: CSocket -> Word32 -> Ptr Word32 -> Word32 -> Ptr () -> Word32
             -> Ptr Word32 -> Ptr () -> Ptr () -> IO CInt
