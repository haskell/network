{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Options (
    SocketOption(SockOpt
                ,UnsupportedSocketOption
                ,AcceptConn,Debug,ReuseAddr,SoDomain,Type,SoProtocol,SoError
                ,DontRoute,Broadcast,SendBuffer,RecvBuffer,KeepAlive,OOBInline
                ,TimeToLive,MaxSegment,NoDelay,Cork,Linger,ReusePort
                ,RecvLowWater,SendLowWater,RecvTimeOut,SendTimeOut
                ,UseLoopBack,UserTimeout,IPv6Only
                ,RecvIPv4TTL,RecvIPv4TOS,RecvIPv4PktInfo
                ,RecvIPv6HopLimit,RecvIPv6TClass,RecvIPv6PktInfo
                ,CustomSockOpt)
  , isSupportedSocketOption
  , whenSupported
  , getSocketType
  , getSocketOption
  , setSocketOption
  , getSockOpt
  , setSockOpt
  , SockOptValue (..)
  , setSockOptValue
  , StructLinger (..)
  , SocketTimeout (..)
  ) where

import qualified Text.Read as P

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types
import Network.Socket.ReadShow

#include <sys/time.h>

----------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
data SocketOption = SockOpt
#if __GLASGOW_HASKELL__ >= 806
    CInt -- ^ Option Level
    CInt -- ^ Option Name
#else
    CInt -- Option Level
    CInt -- Option Name
#endif
  deriving (Eq)

----------------------------------------------------------------

socketOptionBijection :: Bijection SocketOption String
socketOptionBijection =
    [ (UnsupportedSocketOption, "UnsupportedSocketOption")
    , (Debug, "Debug")
    , (ReuseAddr, "ReuseAddr")
    , (SoDomain, "SoDomain")
    , (Type, "Type")
    , (SoProtocol, "SoProtocol")
    , (SoError, "SoError")
    , (DontRoute, "DontRoute")
    , (Broadcast, "Broadcast")
    , (SendBuffer, "SendBuffer")
    , (RecvBuffer, "RecvBuffer")
    , (KeepAlive, "KeepAlive")
    , (OOBInline, "OOBInline")
    , (Linger, "Linger")
    , (ReusePort, "ReusePort")
    , (RecvLowWater, "RecvLowWater")
    , (SendLowWater, "SendLowWater")
    , (RecvTimeOut, "RecvTimeOut")
    , (SendTimeOut, "SendTimeOut")
    , (UseLoopBack, "UseLoopBack")
    , (MaxSegment, "MaxSegment")
    , (NoDelay, "NoDelay")
    , (UserTimeout, "UserTimeout")
    , (Cork, "Cork")
    , (TimeToLive, "TimeToLive")
    , (RecvIPv4TTL, "RecvIPv4TTL")
    , (RecvIPv4TOS, "RecvIPv4TOS")
    , (RecvIPv4PktInfo, "RecvIPv4PktInfo")
    , (IPv6Only, "IPv6Only")
    , (RecvIPv6HopLimit, "RecvIPv6HopLimit")
    , (RecvIPv6TClass, "RecvIPv6TClass")
    , (RecvIPv6PktInfo, "RecvIPv6PktInfo")
    ]

instance Show SocketOption where
    showsPrec = bijectiveShow socketOptionBijection def
      where
        defname = "SockOpt"
        unwrap = \(CustomSockOpt nm) -> nm
        def = defShow defname unwrap showIntInt


instance Read SocketOption where
    readPrec = bijectiveRead socketOptionBijection def
      where
        defname = "SockOpt"
        def = defRead defname CustomSockOpt readIntInt

----------------------------------------------------------------

pattern UnsupportedSocketOption :: SocketOption
pattern UnsupportedSocketOption = SockOpt (-1) (-1)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption opt = opt /= SockOpt (-1) (-1)

-- | Execute the given action only when the specified socket option is
--  supported. Any return value is ignored.
whenSupported :: SocketOption -> IO a -> IO ()
whenSupported s action
  | isSupportedSocketOption s = action >> return ()
  | otherwise                 = return ()

----------------------------------------------------------------

#ifdef SOL_SOCKET
-- | SO_ACCEPTCONN, read-only
pattern AcceptConn :: SocketOption
#ifdef SO_ACCEPTCONN
pattern AcceptConn     = SockOpt (#const SOL_SOCKET) (#const SO_ACCEPTCONN)
#else
pattern AcceptConn     = SockOpt (-1) (-1)
#endif
-- | SO_DEBUG
pattern Debug :: SocketOption
#ifdef SO_DEBUG
pattern Debug          = SockOpt (#const SOL_SOCKET) (#const SO_DEBUG)
#else
pattern Debug          = SockOpt (-1) (-1)
#endif
-- | SO_REUSEADDR
pattern ReuseAddr :: SocketOption
#ifdef SO_REUSEADDR
pattern ReuseAddr      = SockOpt (#const SOL_SOCKET) (#const SO_REUSEADDR)
#else
pattern ReuseAddr      = SockOpt (-1) (-1)
#endif

-- | SO_DOMAIN, read-only
pattern SoDomain :: SocketOption
#ifdef SO_DOMAIN
pattern SoDomain       = SockOpt (#const SOL_SOCKET) (#const SO_DOMAIN)
#else
pattern SoDomain       = SockOpt (-1) (-1)
#endif

-- | SO_TYPE, read-only
pattern Type :: SocketOption
#ifdef SO_TYPE
pattern Type           = SockOpt (#const SOL_SOCKET) (#const SO_TYPE)
#else
pattern Type           = SockOpt (-1) (-1)
#endif

-- | SO_PROTOCOL, read-only
pattern SoProtocol :: SocketOption
#ifdef SO_PROTOCOL
pattern SoProtocol     = SockOpt (#const SOL_SOCKET) (#const SO_PROTOCOL)
#else
pattern SoProtocol     = SockOpt (-1) (-1)
#endif

-- | SO_ERROR
pattern SoError :: SocketOption
#ifdef SO_ERROR
pattern SoError        = SockOpt (#const SOL_SOCKET) (#const SO_ERROR)
#else
pattern SoError        = SockOpt (-1) (-1)
#endif
-- | SO_DONTROUTE
pattern DontRoute :: SocketOption
#ifdef SO_DONTROUTE
pattern DontRoute      = SockOpt (#const SOL_SOCKET) (#const SO_DONTROUTE)
#else
pattern DontRoute      = SockOpt (-1) (-1)
#endif
-- | SO_BROADCAST
pattern Broadcast :: SocketOption
#ifdef SO_BROADCAST
pattern Broadcast      = SockOpt (#const SOL_SOCKET) (#const SO_BROADCAST)
#else
pattern Broadcast      = SockOpt (-1) (-1)
#endif
-- | SO_SNDBUF
pattern SendBuffer :: SocketOption
#ifdef SO_SNDBUF
pattern SendBuffer     = SockOpt (#const SOL_SOCKET) (#const SO_SNDBUF)
#else
pattern SendBuffer     = SockOpt (-1) (-1)
#endif
-- | SO_RCVBUF
pattern RecvBuffer :: SocketOption
#ifdef SO_RCVBUF
pattern RecvBuffer     = SockOpt (#const SOL_SOCKET) (#const SO_RCVBUF)
#else
pattern RecvBuffer     = SockOpt (-1) (-1)
#endif
-- | SO_KEEPALIVE
pattern KeepAlive :: SocketOption
#ifdef SO_KEEPALIVE
pattern KeepAlive      = SockOpt (#const SOL_SOCKET) (#const SO_KEEPALIVE)
#else
pattern KeepAlive      = SockOpt (-1) (-1)
#endif
-- | SO_OOBINLINE
pattern OOBInline :: SocketOption
#ifdef SO_OOBINLINE
pattern OOBInline      = SockOpt (#const SOL_SOCKET) (#const SO_OOBINLINE)
#else
pattern OOBInline      = SockOpt (-1) (-1)
#endif
-- | SO_LINGER: timeout in seconds, 0 means disabling/disabled.
pattern Linger :: SocketOption
#ifdef SO_LINGER
pattern Linger         = SockOpt (#const SOL_SOCKET) (#const SO_LINGER)
#else
pattern Linger         = SockOpt (-1) (-1)
#endif
-- | SO_REUSEPORT
pattern ReusePort :: SocketOption
#ifdef SO_REUSEPORT
pattern ReusePort      = SockOpt (#const SOL_SOCKET) (#const SO_REUSEPORT)
#else
pattern ReusePort      = SockOpt (-1) (-1)
#endif
-- | SO_RCVLOWAT
pattern RecvLowWater :: SocketOption
#ifdef SO_RCVLOWAT
pattern RecvLowWater   = SockOpt (#const SOL_SOCKET) (#const SO_RCVLOWAT)
#else
pattern RecvLowWater   = SockOpt (-1) (-1)
#endif
-- | SO_SNDLOWAT
pattern SendLowWater :: SocketOption
#ifdef SO_SNDLOWAT
pattern SendLowWater   = SockOpt (#const SOL_SOCKET) (#const SO_SNDLOWAT)
#else
pattern SendLowWater   = SockOpt (-1) (-1)
#endif
-- | SO_RCVTIMEO: timeout in microseconds. This option is not useful
-- in the normal case where sockets are non-blocking.
pattern RecvTimeOut :: SocketOption
#ifdef SO_RCVTIMEO
pattern RecvTimeOut    = SockOpt (#const SOL_SOCKET) (#const SO_RCVTIMEO)
#else
pattern RecvTimeOut    = SockOpt (-1) (-1)
#endif
-- | SO_SNDTIMEO: timeout in microseconds. This option is not useful
-- in the normal case where sockets are non-blocking.
pattern SendTimeOut :: SocketOption
#ifdef SO_SNDTIMEO
pattern SendTimeOut    = SockOpt (#const SOL_SOCKET) (#const SO_SNDTIMEO)
#else
pattern SendTimeOut    = SockOpt (-1) (-1)
#endif
-- | SO_USELOOPBACK
pattern UseLoopBack :: SocketOption
#ifdef SO_USELOOPBACK
pattern UseLoopBack    = SockOpt (#const SOL_SOCKET) (#const SO_USELOOPBACK)
#else
pattern UseLoopBack    = SockOpt (-1) (-1)
#endif
#endif // SOL_SOCKET

#if HAVE_DECL_IPPROTO_TCP
-- | TCP_MAXSEG
pattern MaxSegment :: SocketOption
#ifdef TCP_MAXSEG
pattern MaxSegment     = SockOpt (#const IPPROTO_TCP) (#const TCP_MAXSEG)
#else
pattern MaxSegment     = SockOpt (-1) (-1)
#endif
-- | TCP_NODELAY
pattern NoDelay :: SocketOption
#ifdef TCP_NODELAY
pattern NoDelay        = SockOpt (#const IPPROTO_TCP) (#const TCP_NODELAY)
#else
pattern NoDelay        = SockOpt (-1) (-1)
#endif
-- | TCP_USER_TIMEOUT
pattern UserTimeout :: SocketOption
#ifdef TCP_USER_TIMEOUT
pattern UserTimeout    = SockOpt (#const IPPROTO_TCP) (#const TCP_USER_TIMEOUT)
#else
pattern UserTimeout    = SockOpt (-1) (-1)
#endif
-- | TCP_CORK
pattern Cork :: SocketOption
#ifdef TCP_CORK
pattern Cork           = SockOpt (#const IPPROTO_TCP) (#const TCP_CORK)
#else
pattern Cork           = SockOpt (-1) (-1)
#endif
#endif // HAVE_DECL_IPPROTO_TCP

#if HAVE_DECL_IPPROTO_IP
-- | IP_TTL
pattern TimeToLive :: SocketOption
#ifdef IP_TTL
pattern TimeToLive     = SockOpt (#const IPPROTO_IP) (#const IP_TTL)
#else
pattern TimeToLive     = SockOpt (-1) (-1)
#endif
-- | Receiving IPv4 TTL.
pattern RecvIPv4TTL :: SocketOption
#ifdef IP_RECVTTL
pattern RecvIPv4TTL    = SockOpt (#const IPPROTO_IP) (#const IP_RECVTTL)
#else
pattern RecvIPv4TTL    = SockOpt (-1) (-1)
#endif
-- | Receiving IPv4 TOS.
pattern RecvIPv4TOS :: SocketOption
#ifdef IP_RECVTOS
pattern RecvIPv4TOS    = SockOpt (#const IPPROTO_IP) (#const IP_RECVTOS)
#else
pattern RecvIPv4TOS    = SockOpt (-1) (-1)
#endif
-- | Receiving IP_PKTINFO (struct in_pktinfo).
pattern RecvIPv4PktInfo :: SocketOption
#ifdef IP_RECVPKTINFO
pattern RecvIPv4PktInfo  = SockOpt (#const IPPROTO_IP) (#const IP_RECVPKTINFO)
#elif defined(IP_PKTINFO)
pattern RecvIPv4PktInfo  = SockOpt (#const IPPROTO_IP) (#const IP_PKTINFO)
#else
pattern RecvIPv4PktInfo  = SockOpt (-1) (-1)
#endif
#endif // HAVE_DECL_IPPROTO_IP

#if HAVE_DECL_IPPROTO_IPV6
-- | IPV6_V6ONLY: don't use this on OpenBSD.
pattern IPv6Only :: SocketOption
#if HAVE_DECL_IPV6_V6ONLY
pattern IPv6Only       = SockOpt (#const IPPROTO_IPV6) (#const IPV6_V6ONLY)
#else
pattern IPv6Only       = SockOpt (-1) (-1)
#endif
-- | Receiving IPv6 hop limit.
pattern RecvIPv6HopLimit :: SocketOption
#ifdef IPV6_RECVHOPLIMIT
pattern RecvIPv6HopLimit = SockOpt (#const IPPROTO_IPV6) (#const IPV6_RECVHOPLIMIT)
#else
pattern RecvIPv6HopLimit = SockOpt (-1) (-1)
#endif
-- | Receiving IPv6 traffic class.
pattern RecvIPv6TClass :: SocketOption
#ifdef IPV6_RECVTCLASS
pattern RecvIPv6TClass  = SockOpt (#const IPPROTO_IPV6) (#const IPV6_RECVTCLASS)
#else
pattern RecvIPv6TClass  = SockOpt (-1) (-1)
#endif
-- | Receiving IPV6_PKTINFO (struct in6_pktinfo).
pattern RecvIPv6PktInfo :: SocketOption
#ifdef IPV6_RECVPKTINFO
pattern RecvIPv6PktInfo = SockOpt (#const IPPROTO_IPV6) (#const IPV6_RECVPKTINFO)
#elif defined(IPV6_PKTINFO)
pattern RecvIPv6PktInfo = SockOpt (#const IPPROTO_IPV6) (#const IPV6_PKTINFO)
#else
pattern RecvIPv6PktInfo = SockOpt (-1) (-1)
#endif
#endif // HAVE_DECL_IPPROTO_IPV6

pattern CustomSockOpt :: (CInt, CInt) -> SocketOption
pattern CustomSockOpt xy <- ((\(SockOpt x y) -> (x, y)) -> xy)
  where
    CustomSockOpt (x, y) = SockOpt x y

----------------------------------------------------------------

-- | Set a socket option that expects an 'Int' value.
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()
#ifdef SO_LINGER
setSocketOption s so@Linger v = do
    let arg = if v == 0 then StructLinger 0 0 else StructLinger 1 (fromIntegral v)
    setSockOpt s so arg
#endif
setSocketOption s so@RecvTimeOut v = setSockOpt s so $ SocketTimeout $ fromIntegral v
setSocketOption s so@SendTimeOut v = setSockOpt s so $ SocketTimeout $ fromIntegral v
setSocketOption s sa v = setSockOpt s sa (fromIntegral v :: CInt)

-- | Set a socket option.
setSockOpt :: Storable a
           => Socket
           -> SocketOption
           -> a
           -> IO ()
#ifdef HAVE_SETSOCKOPT
setSockOpt s (SockOpt level opt) v = do
    with v $ \ptr -> void $ do
        let sz = fromIntegral $ sizeOf v
        withFdSocket s $ \fd ->
          throwSocketErrorIfMinus1_ "Network.Socket.setSockOpt" $
          c_setsockopt fd level opt ptr sz
#else
setSockOpt _ _ _  = unsupported "setSockOpt"
{-# WARNING setSockOpt "operation will throw 'IOError' \"unsupported operation\"" #-}
#endif

-- | Set a socket option value
--
-- The existential 'SockOptValue' enables things like:
--
-- @
-- mapM_ (uncurry $ 'setSockOptValue' sock) [
--       ('NoDelay', 'SockOptValue' @Int 1)
--     , ('Linger', 'SockOptValue' ('StructLinger' 1 0))
--     ]
-- @
setSockOptValue :: Socket
                     -> SocketOption
                     -> SockOptValue
                     -> IO ()
setSockOptValue s opt (SockOptValue v) = setSockOpt s opt v

----------------------------------------------------------------

-- | Get a socket option that gives an 'Int' value.
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value
#ifdef SO_LINGER
getSocketOption s so@Linger = do
    StructLinger onoff linger <- getSockOpt s so
    return $ fromIntegral $ if onoff == 0 then 0 else linger
#endif
getSocketOption s so@RecvTimeOut = do
    SocketTimeout to <- getSockOpt s so
    return $ fromIntegral to
getSocketOption s so@SendTimeOut = do
    SocketTimeout to <- getSockOpt s so
    return $ fromIntegral to
getSocketOption s so = do
    n :: CInt <- getSockOpt s so
    return $ fromIntegral n

-- | Get a socket option.
getSockOpt :: forall a . Storable a
           => Socket
           -> SocketOption -- Option Name
           -> IO a         -- Option Value
getSockOpt s (SockOpt level opt) = do
    alloca $ \ptr -> do
        let sz = fromIntegral $ sizeOf (undefined :: a)
        withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
            throwSocketErrorIfMinus1Retry_ "Network.Socket.getSockOpt" $
                c_getsockopt fd level opt ptr ptr_sz
        peek ptr

----------------------------------------------------------------

-- | Get the 'SocketType' of an active socket.
--
--   Since: 3.0.1.0
getSocketType :: Socket -> IO SocketType
getSocketType s = unpackSocketType <$> getSockOpt s Type

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 806
{-# COMPLETE CustomSockOpt #-}
#endif

-- | Low level @SO_LINGER@ option value, which can be used with 'setSockOpt' or
-- @'setSockOptValue' . 'SockOptValue'@.
data StructLinger = StructLinger {
    -- | Set the linger option on.
    sl_onoff  :: CInt,

    -- | Linger timeout.
    sl_linger :: CInt
  }
  deriving (Eq, Ord, Show)

#ifdef SO_LINGER
instance Storable StructLinger where
    sizeOf    ~_ = (#const sizeof(struct linger))
    alignment ~_ = alignment (0 :: CInt)

    peek p = do
        onoff  <- (#peek struct linger, l_onoff) p
        linger <- (#peek struct linger, l_linger) p
        return $ StructLinger onoff linger

    poke p (StructLinger onoff linger) = do
        (#poke struct linger, l_onoff)  p onoff
        (#poke struct linger, l_linger) p linger
#endif

-- | A type that can hold any 'Storable' socket option value (e.g.
-- 'StructLinger' and 'CInt')
--
-- See 'setSocOptValue'
data SockOptValue where
  SockOptValue :: Storable a => a -> SockOptValue

----------------------------------------------------------------

-- | Timeout in microseconds.
--   This will be converted into struct timeval on Unix and
--   DWORD (as milliseconds) on Windows.
newtype SocketTimeout = SocketTimeout Word32 deriving (Eq, Ord, Show)

#if defined(mingw32_HOST_OS)
instance Storable SocketTimeout where
    sizeOf ~(SocketTimeout to) = sizeOf to -- DWORD as milliseconds
    alignment ~_ = 0
    peek ptr    = do
        to <- peek (castPtr ptr)
        return $ SocketTimeout (to * 1000)
    poke ptr (SocketTimeout to) = poke (castPtr ptr) (to `div` 1000)
#else
instance Storable SocketTimeout where
    sizeOf    ~_ = (#size struct timeval)
    alignment ~_ = (#const offsetof(struct {char x__; struct timeval (y__); }, y__))
    peek ptr    = do
            sec  <- (#peek struct timeval, tv_sec)  ptr
            usec <- (#peek struct timeval, tv_usec) ptr
            return $ SocketTimeout (sec * 1000000 + usec)
    poke ptr (SocketTimeout to) = do
            let (sec, usec) = to `divMod` 1000000
            (#poke struct timeval, tv_sec)  ptr sec
            (#poke struct timeval, tv_usec) ptr usec
#endif

----------------------------------------------------------------

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
#ifdef HAVE_SETSOCKOPT
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
#endif
