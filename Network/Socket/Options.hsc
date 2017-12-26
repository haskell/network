{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Options where

import Control.Monad (liftM)
import Data.Maybe (isJust)
import Data.Typeable
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

#if defined(HAVE_STRUCT_UCRED_SO_PEERCRED)
import Foreign.Marshal.Alloc (allocaBytes)
#endif
#if defined(HAVE_STRUCT_UCRED_SO_PEERCRED) || defined(HAVE_GETPEEREID)
import Foreign.C.Types (CUInt(..))
#endif

import Network.Socket.Internal
import Network.Socket.Types

-----------------------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
data SocketOption
    = Debug         -- ^ SO_DEBUG
    | ReuseAddr     -- ^ SO_REUSEADDR
    | Type          -- ^ SO_TYPE
    | SoError       -- ^ SO_ERROR
    | DontRoute     -- ^ SO_DONTROUTE
    | Broadcast     -- ^ SO_BROADCAST
    | SendBuffer    -- ^ SO_SNDBUF
    | RecvBuffer    -- ^ SO_RCVBUF
    | KeepAlive     -- ^ SO_KEEPALIVE
    | OOBInline     -- ^ SO_OOBINLINE
    | TimeToLive    -- ^ IP_TTL
    | MaxSegment    -- ^ TCP_MAXSEG
    | NoDelay       -- ^ TCP_NODELAY
    | Cork          -- ^ TCP_CORK
    | Linger        -- ^ SO_LINGER
    | ReusePort     -- ^ SO_REUSEPORT
    | RecvLowWater  -- ^ SO_RCVLOWAT
    | SendLowWater  -- ^ SO_SNDLOWAT
    | RecvTimeOut   -- ^ SO_RCVTIMEO
    | SendTimeOut   -- ^ SO_SNDTIMEO
    | UseLoopBack   -- ^ SO_USELOOPBACK
    | UserTimeout   -- ^ TCP_USER_TIMEOUT
    | IPv6Only      -- ^ IPV6_V6ONLY
    | CustomSockOpt (CInt, CInt)
    deriving (Show, Typeable)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption = isJust . packSocketOption

-- | For a socket option, return Just (level, value) where level is the
-- corresponding C option level constant (e.g. SOL_SOCKET) and value is
-- the option constant itself (e.g. SO_DEBUG)
-- If either constant does not exist, return Nothing.
packSocketOption :: SocketOption -> Maybe (CInt, CInt)
packSocketOption so =
  -- The Just here is a hack to disable GHC's overlapping pattern detection:
  -- the problem is if all constants are present, the fallback pattern is
  -- redundant, but if they aren't then it isn't. Hence we introduce an
  -- extra pattern (Nothing) that can't possibly happen, so that the
  -- fallback is always (in principle) necessary.
  -- I feel a little bad for including this, but such are the sacrifices we
  -- make while working with CPP - excluding the fallback pattern correctly
  -- would be a serious nuisance.
  -- (NB: comments elsewhere in this file refer to this one)
  case Just so of
#ifdef SOL_SOCKET
#ifdef SO_DEBUG
    Just Debug         -> Just ((#const SOL_SOCKET), (#const SO_DEBUG))
#endif
#ifdef SO_REUSEADDR
    Just ReuseAddr     -> Just ((#const SOL_SOCKET), (#const SO_REUSEADDR))
#endif
#ifdef SO_TYPE
    Just Type          -> Just ((#const SOL_SOCKET), (#const SO_TYPE))
#endif
#ifdef SO_ERROR
    Just SoError       -> Just ((#const SOL_SOCKET), (#const SO_ERROR))
#endif
#ifdef SO_DONTROUTE
    Just DontRoute     -> Just ((#const SOL_SOCKET), (#const SO_DONTROUTE))
#endif
#ifdef SO_BROADCAST
    Just Broadcast     -> Just ((#const SOL_SOCKET), (#const SO_BROADCAST))
#endif
#ifdef SO_SNDBUF
    Just SendBuffer    -> Just ((#const SOL_SOCKET), (#const SO_SNDBUF))
#endif
#ifdef SO_RCVBUF
    Just RecvBuffer    -> Just ((#const SOL_SOCKET), (#const SO_RCVBUF))
#endif
#ifdef SO_KEEPALIVE
    Just KeepAlive     -> Just ((#const SOL_SOCKET), (#const SO_KEEPALIVE))
#endif
#ifdef SO_OOBINLINE
    Just OOBInline     -> Just ((#const SOL_SOCKET), (#const SO_OOBINLINE))
#endif
#ifdef SO_LINGER
    Just Linger        -> Just ((#const SOL_SOCKET), (#const SO_LINGER))
#endif
#ifdef SO_REUSEPORT
    Just ReusePort     -> Just ((#const SOL_SOCKET), (#const SO_REUSEPORT))
#endif
#ifdef SO_RCVLOWAT
    Just RecvLowWater  -> Just ((#const SOL_SOCKET), (#const SO_RCVLOWAT))
#endif
#ifdef SO_SNDLOWAT
    Just SendLowWater  -> Just ((#const SOL_SOCKET), (#const SO_SNDLOWAT))
#endif
#ifdef SO_RCVTIMEO
    Just RecvTimeOut   -> Just ((#const SOL_SOCKET), (#const SO_RCVTIMEO))
#endif
#ifdef SO_SNDTIMEO
    Just SendTimeOut   -> Just ((#const SOL_SOCKET), (#const SO_SNDTIMEO))
#endif
#ifdef SO_USELOOPBACK
    Just UseLoopBack   -> Just ((#const SOL_SOCKET), (#const SO_USELOOPBACK))
#endif
#endif // SOL_SOCKET
#if HAVE_DECL_IPPROTO_IP
#ifdef IP_TTL
    Just TimeToLive    -> Just ((#const IPPROTO_IP), (#const IP_TTL))
#endif
#endif // HAVE_DECL_IPPROTO_IP
#if HAVE_DECL_IPPROTO_TCP
#ifdef TCP_MAXSEG
    Just MaxSegment    -> Just ((#const IPPROTO_TCP), (#const TCP_MAXSEG))
#endif
#ifdef TCP_NODELAY
    Just NoDelay       -> Just ((#const IPPROTO_TCP), (#const TCP_NODELAY))
#endif
#ifdef TCP_USER_TIMEOUT
    Just UserTimeout   -> Just ((#const IPPROTO_TCP), (#const TCP_USER_TIMEOUT))
#endif
#ifdef TCP_CORK
    Just Cork          -> Just ((#const IPPROTO_TCP), (#const TCP_CORK))
#endif
#endif // HAVE_DECL_IPPROTO_TCP
#if HAVE_DECL_IPPROTO_IPV6
#if HAVE_DECL_IPV6_V6ONLY
    Just IPv6Only      -> Just ((#const IPPROTO_IPV6), (#const IPV6_V6ONLY))
#endif
#endif // HAVE_DECL_IPPROTO_IPV6
    Just (CustomSockOpt opt) -> Just opt
    _             -> Nothing

-- | Return the option level and option value if they exist,
-- otherwise throw an error that begins "Network.Socket." ++ the String
-- parameter
packSocketOption' :: String -> SocketOption -> IO (CInt, CInt)
packSocketOption' caller so = maybe err return (packSocketOption so)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller,
    ": socket option ", show so, " unsupported on this system"]

-- | Set a socket option that expects an Int value.
-- There is currently no API to set e.g. the timeval socket options
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()
setSocketOption Socket{..} so v = do
   (level, opt) <- packSocketOption' "setSocketOption" so
   with (fromIntegral v) $ \ptr_v -> do
   throwSocketErrorIfMinus1_ "Network.Socket.setSocketOption" $
       c_setsockopt socketFd level opt ptr_v
          (fromIntegral (sizeOf (undefined :: CInt)))
   return ()


-- | Get a socket option that gives an Int value.
-- There is currently no API to get e.g. the timeval socket options
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value
getSocketOption Socket{..} so = do
   (level, opt) <- packSocketOption' "getSocketOption" so
   alloca $ \ptr_v ->
     with (fromIntegral (sizeOf (undefined :: CInt))) $ \ptr_sz -> do
       throwSocketErrorIfMinus1Retry_ "Network.Socket.getSocketOption" $
         c_getsockopt socketFd level opt ptr_v ptr_sz
       fromIntegral `liftM` peek ptr_v


#if defined(HAVE_STRUCT_UCRED_SO_PEERCRED) || defined(HAVE_GETPEEREID)
-- | Returns the processID, userID and groupID of the peer of
--   a UNIX domain socket.
--
-- Only available on platforms that support SO_PEERCRED or 'getPeerEid'.
-- If 'getPeerEid' is used, processID is always 0.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred sock = do
#ifdef HAVE_STRUCT_UCRED_SO_PEERCRED
  let fd = socketFd sock
  let sz = (#const sizeof(struct ucred))
  allocaBytes sz $ \ ptr_cr ->
   with (fromIntegral sz) $ \ ptr_sz -> do
     _ <- ($) throwSocketErrorIfMinus1Retry "Network.Socket.getPeerCred" $
       c_getsockopt fd (#const SOL_SOCKET) (#const SO_PEERCRED) ptr_cr ptr_sz
     pid <- (#peek struct ucred, pid) ptr_cr
     uid <- (#peek struct ucred, uid) ptr_cr
     gid <- (#peek struct ucred, gid) ptr_cr
     return (pid, uid, gid)
#else
  (uid,gid) <- getPeerEid sock
  return (0,uid,gid)
#endif

#ifdef HAVE_GETPEEREID
-- | Returns the userID and groupID of the peer of
--   a UNIX domain socket.
--
--  Only available on platforms that support getpeereid().
getPeerEid :: Socket -> IO (CUInt, CUInt)
getPeerEid sock = do
  let fd = socketFd sock
  alloca $ \ ptr_uid ->
    alloca $ \ ptr_gid -> do
      throwSocketErrorIfMinus1Retry_ "Network.Socket.getPeerEid" $
        c_getpeereid fd ptr_uid ptr_gid
      uid <- peek ptr_uid
      gid <- peek ptr_gid
      return (uid, gid)
#endif
#endif

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
#if defined(HAVE_GETPEEREID)
foreign import CALLCONV unsafe "getpeereid"
  c_getpeereid :: CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt
#endif
