{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Unix (
    isUnixDomainSocketAvailable
  , socketPair
  , sendFd
  , recvFd
  , getPeerCredential
  , getPeerCred
  , getPeerEid
  ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Network.Socket.Buffer
import Network.Socket.Fcntl
import Network.Socket.Imports
import Network.Socket.Types
import System.Posix.Types (Fd(..))

#if defined(mingw32_HOST_OS)
import Network.Socket.Syscall
import Network.Socket.Win32.Cmsg
import System.Directory
import System.IO
import System.IO.Temp
#else
import Foreign.Marshal.Array (peekArray)
import Network.Socket.Internal
import Network.Socket.Posix.Cmsg
#endif

#if defined(HAVE_GETPEEREID)
import System.IO.Error (catchIOError)
#endif
#ifdef HAVE_GETPEEREID
import Foreign.Marshal.Alloc (alloca)
#endif

#ifdef HAVE_STRUCT_UCRED_SO_PEERCRED
import Network.Socket.Options
#endif

-- | Getting process ID, user ID and group ID for UNIX-domain sockets.
--
--   This is implemented with SO_PEERCRED on Linux and getpeereid()
--   on BSD variants. Unfortunately, on some BSD variants
--   getpeereid() returns unexpected results, rather than an error,
--   for AF_INET sockets. It is the user's responsibility to make sure
--   that the socket is a UNIX-domain socket.
--   Also, on some BSD variants, getpeereid() does not return credentials
--   for sockets created via 'socketPair', only separately created and then
--   explicitly connected UNIX-domain sockets work on such systems.
--
--   Since 2.7.0.0.
getPeerCredential :: Socket -> IO (Maybe CUInt, Maybe CUInt, Maybe CUInt)
#ifdef HAVE_STRUCT_UCRED_SO_PEERCRED
getPeerCredential sock = do
    (pid, uid, gid) <- getPeerCred sock
    if uid == maxBound then
        return (Nothing, Nothing, Nothing)
      else
        return (Just pid, Just uid, Just gid)
#elif defined(HAVE_GETPEEREID)
getPeerCredential sock =
    go `catchIOError` \_ -> return (Nothing,Nothing,Nothing)
  where
    go = do
        (uid, gid) <- getPeerEid sock
        return (Nothing, Just uid, Just gid)
#else
getPeerCredential _ = return (Nothing, Nothing, Nothing)
#endif

-- | Returns the processID, userID and groupID of the peer of
--   a UNIX-domain socket.
--
-- Only available on platforms that support SO_PEERCRED.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
#ifdef HAVE_STRUCT_UCRED_SO_PEERCRED
getPeerCred s = do
    let opt = SockOpt (#const SOL_SOCKET) (#const SO_PEERCRED)
    PeerCred cred <- getSockOpt s opt
    return cred

newtype PeerCred = PeerCred (CUInt, CUInt, CUInt)
instance Storable PeerCred where
    sizeOf    _ = (#const sizeof(struct ucred))
    alignment _ = alignment (0 :: CInt)
    poke _ _ = return ()
    peek p = do
        pid <- (#peek struct ucred, pid) p
        uid <- (#peek struct ucred, uid) p
        gid <- (#peek struct ucred, gid) p
        return $ PeerCred (pid, uid, gid)
#else
getPeerCred _ = return (0, 0, 0)
#endif
{-# Deprecated getPeerCred "Use getPeerCredential instead" #-}

-- | Returns the userID and groupID of the peer of
--   a UNIX-domain socket.
--
--  Only available on platforms that support getpeereid().
getPeerEid :: Socket -> IO (CUInt, CUInt)
#ifdef HAVE_GETPEEREID
getPeerEid s = do
  alloca $ \ ptr_uid ->
    alloca $ \ ptr_gid -> do
      withFdSocket s $ \fd ->
        throwSocketErrorIfMinus1Retry_ "Network.Socket.getPeerEid" $
          c_getpeereid fd ptr_uid ptr_gid
      uid <- peek ptr_uid
      gid <- peek ptr_gid
      return (uid, gid)

foreign import CALLCONV unsafe "getpeereid"
  c_getpeereid :: CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt
#else
getPeerEid _ = return (0, 0)
#endif

{-# Deprecated getPeerEid "Use getPeerCredential instead" #-}

-- | Whether or not UNIX-domain sockets are available.
--   'AF_UNIX' is supported on Windows since 3.1.3.0.
--   So, this variable is 'True` on all platforms.
--
--   Since 2.7.0.0.
isUnixDomainSocketAvailable :: Bool
isUnixDomainSocketAvailable = True

data NullSockAddr = NullSockAddr

instance SocketAddress NullSockAddr where
    sizeOfSocketAddress _ = 0
    peekSocketAddress _   = return NullSockAddr
    pokeSocketAddress _ _ = return ()

-- | Send a file descriptor over a UNIX-domain socket.
--   This function does not work on Windows.
sendFd :: Socket -> CInt -> IO ()
sendFd s outfd = void $ allocaBytes dummyBufSize $ \buf -> do
    let cmsg = encodeCmsg $ Fd outfd
    sendBufMsg s NullSockAddr [(buf,dummyBufSize)] [cmsg] mempty
  where
    dummyBufSize = 1

-- | Receive a file descriptor over a UNIX-domain socket. Note that the resulting
--   file descriptor may have to be put into non-blocking mode in order to be
--   used safely. See 'setNonBlockIfNeeded'.
--   This function does not work on Windows.
recvFd :: Socket -> IO CInt
recvFd s = allocaBytes dummyBufSize $ \buf -> do
    (NullSockAddr, _, cmsgs, _) <- recvBufMsg s [(buf,dummyBufSize)] 32 mempty
    case (lookupCmsg CmsgIdFd cmsgs >>= decodeCmsg) :: Maybe Fd of
      Nothing      -> return (-1)
      Just (Fd fd) -> return fd
  where
    dummyBufSize = 16

-- | Build a pair of connected socket objects.
--   On Windows, this function emulates socketpair() using
--   'AF_UNIX' and a temporary file will remain.
socketPair :: Family              -- Family Name (usually AF_UNIX)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
#if defined(mingw32_HOST_OS)
socketPair _ _ _ = withSystemTempFile "temp-for-pair" $ \file hdl -> do
    hClose hdl
    removeFile file
    listenSock <- socket AF_UNIX Stream defaultProtocol
    bind listenSock $ SockAddrUnix file
    listen listenSock 10
    clientSock <- socket AF_UNIX Stream defaultProtocol
    connect clientSock $ SockAddrUnix file
    (serverSock, _ :: SockAddr) <- accept listenSock
    close listenSock
    withFdSocket clientSock setNonBlockIfNeeded
    withFdSocket serverSock setNonBlockIfNeeded
    return (clientSock, serverSock)
#else
socketPair family stype protocol =
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
      let c_stype = packSocketType stype
      _rc <- throwSocketErrorIfMinus1Retry "Network.Socket.socketpair" $
                  c_socketpair (packFamily family) c_stype protocol fdArr
      [fd1,fd2] <- peekArray 2 fdArr
      setNonBlockIfNeeded fd1
      setNonBlockIfNeeded fd2
      s1 <- mkSocket fd1
      s2 <- mkSocket fd2
      return (s1, s2)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
#endif
