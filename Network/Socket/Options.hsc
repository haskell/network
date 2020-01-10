{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Options (
    SocketOption(Debug,ReuseAddr,Type,SoError,DontRoute,Broadcast
                ,SendBuffer,RecvBuffer,KeepAlive,OOBInline,TimeToLive
                ,MaxSegment,NoDelay,Cork,Linger,ReusePort
                ,RecvLowWater,SendLowWater,RecvTimeOut,SendTimeOut
                ,UseLoopBack,UserTimeout,IPv6Only
                ,RecvIPv4TTL,RecvIPv4TOS,RecvIPv4PktInfo
                ,RecvIPv6HopLimit,RecvIPv6TClass,RecvIPv6PktInfo
                ,CustomSockOpt)
  , isSupportedSocketOption
  , getSocketType
  , getSocketOption
  , setSocketOption
  , getSockOpt
  , setSockOpt
  , c_getsockopt
  , c_setsockopt
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

-----------------------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
newtype SocketOption = SockOpt (CInt,CInt) deriving (Eq, Show)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption opt = opt /= SockOpt (-1,-1)

-- | Get the 'SocketType' of an active socket.
--
--   Since: 3.0.1.0
getSocketType :: Socket -> IO SocketType
getSocketType s = (fromMaybe NoSocketType . unpackSocketType . fromIntegral)
                    <$> getSocketOption s Type

#ifdef SOL_SOCKET
-- | SO_DEBUG
pattern Debug :: SocketOption
#ifdef SO_DEBUG
pattern Debug          = SockOpt ((#const SOL_SOCKET), (#const SO_DEBUG))
#else
pattern Debug          = SockOpt (-1,-1)
#endif
-- | SO_REUSEADDR
pattern ReuseAddr :: SocketOption
#ifdef SO_REUSEADDR
pattern ReuseAddr      = SockOpt ((#const SOL_SOCKET), (#const SO_REUSEADDR))
#else
pattern ReuseAddr      = SockOpt (-1,-1)
#endif
-- | SO_TYPE
pattern Type :: SocketOption
#ifdef SO_TYPE
pattern Type           = SockOpt ((#const SOL_SOCKET), (#const SO_TYPE))
#else
pattern Type           = SockOpt (-1,-1)
#endif
-- | SO_ERROR
pattern SoError :: SocketOption
#ifdef SO_ERROR
pattern SoError        = SockOpt ((#const SOL_SOCKET), (#const SO_ERROR))
#else
pattern SoError        = SockOpt (-1,-1)
#endif
-- | SO_DONTROUTE
pattern DontRoute :: SocketOption
#ifdef SO_DONTROUTE
pattern DontRoute      = SockOpt ((#const SOL_SOCKET), (#const SO_DONTROUTE))
#else
pattern DontRoute      = SockOpt (-1,-1)
#endif
-- | SO_BROADCAST
pattern Broadcast :: SocketOption
#ifdef SO_BROADCAST
pattern Broadcast      = SockOpt ((#const SOL_SOCKET), (#const SO_BROADCAST))
#else
pattern Broadcast      = SockOpt (-1,-1)
#endif
-- | SO_SNDBUF
pattern SendBuffer :: SocketOption
#ifdef SO_SNDBUF
pattern SendBuffer     = SockOpt ((#const SOL_SOCKET), (#const SO_SNDBUF))
#else
pattern SendBuffer     = SockOpt (-1,-1)
#endif
-- | SO_RCVBUF
pattern RecvBuffer :: SocketOption
#ifdef SO_RCVBUF
pattern RecvBuffer     = SockOpt ((#const SOL_SOCKET), (#const SO_RCVBUF))
#else
pattern RecvBuffer     = SockOpt (-1,-1)
#endif
-- | SO_KEEPALIVE
pattern KeepAlive :: SocketOption
#ifdef SO_KEEPALIVE
pattern KeepAlive      = SockOpt ((#const SOL_SOCKET), (#const SO_KEEPALIVE))
#else
pattern KeepAlive      = SockOpt (-1,-1)
#endif
-- | SO_OOBINLINE
pattern OOBInline :: SocketOption
#ifdef SO_OOBINLINE
pattern OOBInline      = SockOpt ((#const SOL_SOCKET), (#const SO_OOBINLINE))
#else
pattern OOBINLINE      = SockOpt (-1,-1)
#endif
-- | SO_LINGER: timeout in seconds, 0 means disabling/disabled.
pattern Linger :: SocketOption
#ifdef SO_LINGER
pattern Linger         = SockOpt ((#const SOL_SOCKET), (#const SO_LINGER))
#else
pattern Linger         = SockOpt (-1,-1)
#endif
-- | SO_REUSEPORT
pattern ReusePort :: SocketOption
#ifdef SO_REUSEPORT
pattern ReusePort      = SockOpt ((#const SOL_SOCKET), (#const SO_REUSEPORT))
#else
pattern ReusePort      = SockOpt (-1,-1)
#endif
-- | SO_RCVLOWAT
pattern RecvLowWater :: SocketOption
#ifdef SO_RCVLOWAT
pattern RecvLowWater   = SockOpt ((#const SOL_SOCKET), (#const SO_RCVLOWAT))
#else
pattern RecvLowWater   = SockOpt (-1,-1)
#endif
-- | SO_SNDLOWAT
pattern SendLowWater :: SocketOption
#ifdef SO_SNDLOWAT
pattern SendLowWater   = SockOpt ((#const SOL_SOCKET), (#const SO_SNDLOWAT))
#else
pattern SendLowWater   = SockOpt (-1,-1)
#endif
-- | SO_RCVTIMEO: this does not work at this moment.
pattern RecvTimeOut :: SocketOption
#ifdef SO_RCVTIMEO
pattern RecvTimeOut    = SockOpt ((#const SOL_SOCKET), (#const SO_RCVTIMEO))
#else
pattern RecvTimeOut    = SockOpt (-1,-1)
#endif
-- | SO_SNDTIMEO: this does not work at this moment.
pattern SendTimeOut :: SocketOption
#ifdef SO_SNDTIMEO
pattern SendTimeOut    = SockOpt ((#const SOL_SOCKET), (#const SO_SNDTIMEO))
#else
pattern SendTimeOut    = SockOpt (-1,-1)
#endif
-- | SO_USELOOPBACK
pattern UseLoopBack :: SocketOption
#ifdef SO_USELOOPBACK
pattern UseLoopBack    = SockOpt ((#const SOL_SOCKET), (#const SO_USELOOPBACK))
#else
pattern UseLoopBack    = SockOpt (-1,-1)
#endif
#endif // SOL_SOCKET

#if HAVE_DECL_IPPROTO_TCP
-- | TCP_MAXSEG
pattern MaxSegment :: SocketOption
#ifdef TCP_MAXSEG
pattern MaxSegment     = SockOpt ((#const IPPROTO_TCP), (#const TCP_MAXSEG))
#else
pattern MaxSegment     = SockOpt (-1,-1)
#endif
-- | TCP_NODELAY
pattern NoDelay :: SocketOption
#ifdef TCP_NODELAY
pattern NoDelay        = SockOpt ((#const IPPROTO_TCP), (#const TCP_NODELAY))
#else
pattern NoDelay        = SockOpt (-1,-1)
#endif
-- | TCP_USER_TIMEOUT
pattern UserTimeout :: SocketOption
#ifdef TCP_USER_TIMEOUT
pattern UserTimeout    = SockOpt ((#const IPPROTO_TCP), (#const TCP_USER_TIMEOUT))
#else
pattern UserTimeout    = SockOpt (-1, -1)
#endif
-- | TCP_CORK
pattern Cork :: SocketOption
#ifdef TCP_CORK
pattern Cork           = SockOpt ((#const IPPROTO_TCP), (#const TCP_CORK))
#else
pattern Cork           = SockOpt (-1,-1)
#endif
#endif // HAVE_DECL_IPPROTO_TCP

#if HAVE_DECL_IPPROTO_IP
-- | IP_TTL
pattern TimeToLive :: SocketOption
#ifdef IP_TTL
pattern TimeToLive     = SockOpt ((#const IPPROTO_IP), (#const IP_TTL))
#else
pattern TimeToLive     = SockOpt (-1,-1)
#endif
-- | Receiving IPv4 TTL.
pattern RecvIPv4TTL :: SocketOption
#ifdef IP_RECVTTL
pattern RecvIPv4TTL    = SockOpt ((#const IPPROTO_IP), (#const IP_RECVTTL))
#else
pattern RecvIPv4TTL    = SockOpt (-1,-1)
#endif
-- | Receiving IPv4 TOS.
pattern RecvIPv4TOS :: SocketOption
#ifdef IP_RECVTOS
pattern RecvIPv4TOS    = SockOpt ((#const IPPROTO_IP), (#const IP_RECVTOS))
#else
pattern RecvIPv4TOS    = SockOpt (-1,-1)
#endif
-- | Receiving IP_PKTINFO (struct in_pktinfo).
pattern RecvIPv4PktInfo :: SocketOption
#ifdef IP_RECVPKTINFO
pattern RecvIPv4PktInfo  = SockOpt ((#const IPPROTO_IP), (#const IP_RECVPKTINFO))
#elif defined(IP_PKTINFO)
pattern RecvIPv4PktInfo  = SockOpt ((#const IPPROTO_IP), (#const IP_PKTINFO))
#else
pattern RecvIPv4PktInfo  = SockOpt (-1,-1)
#endif
#endif // HAVE_DECL_IPPROTO_IP

#if HAVE_DECL_IPPROTO_IPV6
-- | IPV6_V6ONLY: don't use this on OpenBSD.
pattern IPv6Only :: SocketOption
#if HAVE_DECL_IPV6_V6ONLY
pattern IPv6Only       = SockOpt ((#const IPPROTO_IPV6), (#const IPV6_V6ONLY))
#else
pattern IPv6Only       = SockOpt (-1,-1)
#endif
-- | Receiving IPv6 hop limit.
pattern RecvIPv6HopLimit :: SocketOption
#ifdef IPV6_RECVHOPLIMIT
pattern RecvIPv6HopLimit = SockOpt ((#const IPPROTO_IPV6), (#const IPV6_RECVHOPLIMIT))
#else
pattern RecvIPv6HopLimit = SockOpt (-1,-1)
#endif
-- | Receiving IPv6 traffic class.
pattern RecvIPv6TClass :: SocketOption
#ifdef IPV6_RECVTCLASS
pattern RecvIPv6TClass  = SockOpt ((#const IPPROTO_IPV6), (#const IPV6_RECVTCLASS))
#else
pattern RecvIPv6TClass  = SockOpt (-1,-1)
#endif
-- | Receiving IPV6_PKTINFO (struct in6_pktinfo).
pattern RecvIPv6PktInfo :: SocketOption
#ifdef IPV6_RECVPKTINFO
pattern RecvIPv6PktInfo = SockOpt ((#const IPPROTO_IPV6), (#const IPV6_RECVPKTINFO))
#elif defined(IPV6_PKTINFO)
pattern RecvIPv6PktInfo = SockOpt ((#const IPPROTO_IPV6), (#const IPV6_PKTINFO))
#else
pattern RecvIPv6PktInfo = SockOpt (-1,-1)
#endif
#endif // HAVE_DECL_IPPROTO_IPV6

-- | Customizable socket option.
pattern CustomSockOpt :: (CInt,CInt) -> SocketOption
pattern CustomSockOpt opt = SockOpt opt

#ifdef SO_LINGER
data StructLinger = StructLinger CInt CInt

instance Storable StructLinger where
    sizeOf _ = (#const sizeof(struct linger))
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        onoff  <- (#peek struct linger, l_onoff) p
        linger <- (#peek struct linger, l_linger) p
        return $ StructLinger onoff linger

    poke p (StructLinger onoff linger) = do
        (#poke struct linger, l_onoff)  p onoff
        (#poke struct linger, l_linger) p linger
#endif

-- | Set a socket option that expects an Int value.
-- There is currently no API to set e.g. the timeval socket options
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()
#ifdef SO_LINGER
setSocketOption s so@Linger v = do
    let arg = if v == 0 then StructLinger 0 0 else StructLinger 1 (fromIntegral v)
    setSockOpt s so arg
#endif
setSocketOption s sa v = setSockOpt s sa (fromIntegral v :: CInt)

-- | Set a socket option.
setSockOpt :: Storable a
           => Socket
           -> SocketOption
           -> a
           -> IO ()
setSockOpt s (SockOpt (level,opt)) v = do
    with v $ \ptr -> void $ do
        let sz = fromIntegral $ sizeOf v
        withFdSocket s $ \fd ->
          throwSocketErrorIfMinus1_ "Network.Socket.setSockOpt" $
          c_setsockopt fd level opt ptr sz

-- | Get a socket option that gives an Int value.
-- There is currently no API to get e.g. the timeval socket options
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value
#ifdef SO_LINGER
getSocketOption s so@Linger = do
    StructLinger onoff linger <- getSockOpt s so
    return $ fromIntegral $ if onoff == 0 then 0 else linger
#endif
getSocketOption s so = do
    n :: CInt <- getSockOpt s so
    return $ fromIntegral n

-- | Get a socket option.
getSockOpt :: forall a . Storable a
           => Socket
           -> SocketOption  -- Option Name
           -> IO a        -- Option Value
getSockOpt s (SockOpt (level,opt)) = do
    alloca $ \ptr -> do
        let sz = fromIntegral $ sizeOf (undefined :: a)
        withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
            throwSocketErrorIfMinus1Retry_ "Network.Socket.getSockOpt" $
                c_getsockopt fd level opt ptr ptr_sz
        peek ptr

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
