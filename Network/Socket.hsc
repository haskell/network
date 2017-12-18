{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).
--
-- Here are two minimal example programs using the TCP/IP protocol: a
-- server that echoes all data that it receives back (servicing only
-- one client) and a client using it.
--
-- > -- Echo server program
-- > module Main (main) where
-- >
-- > import Control.Concurrent (forkFinally)
-- > import qualified Control.Exception as E
-- > import Control.Monad (unless, forever, void)
-- > import qualified Data.ByteString as S
-- > import Network.Socket hiding (recv)
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >     addr <- resolve "3000"
-- >     E.bracket (open addr) close loop
-- >   where
-- >     resolve port = do
-- >         let hints = defaultHints {
-- >                 addrFlags = [AI_PASSIVE]
-- >               , addrSocketType = Stream
-- >               }
-- >         addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
-- >         return addr
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         setSocketOption sock ReuseAddr 1
-- >         bind sock (addrAddress addr)
-- >         listen sock 10
-- >         return sock
-- >     loop sock = forever $ do
-- >         (conn, peer) <- accept sock
-- >         putStrLn $ "Connection from " ++ show peer
-- >         void $ forkFinally (talk conn) (\_ -> close conn)
-- >     talk conn = do
-- >         msg <- recv conn 1024
-- >         unless (S.null msg) $ do
-- >           sendAll conn msg
-- >           talk conn
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > -- Echo client program
-- > module Main (main) where
-- >
-- > import qualified Control.Exception as E
-- > import qualified Data.ByteString.Char8 as C
-- > import Network.Socket hiding (recv)
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >     addr <- resolve "127.0.0.1" "3000"
-- >     E.bracket (open addr) close talk
-- >   where
-- >     resolve host port = do
-- >         let hints = defaultHints { addrSocketType = Stream }
-- >         addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
-- >         return addr
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         connect sock $ addrAddress addr
-- >         return sock
-- >     talk sock = do
-- >         sendAll sock "Hello, world!"
-- >         msg <- recv sock 1024
-- >         putStr "Received: "
-- >         C.putStrLn msg
-----------------------------------------------------------------------------

#include "HsNet.h"

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket
    (
    -- * Types
    -- ** Socket
      Socket(..)
    , SocketStatus(..)
    -- ** Family
    , Family(..)
    , isSupportedFamily
    -- ** Socket type
    , SocketType(..)
    , isSupportedSocketType
    -- ** Socket address
    , SockAddr(..)
    , isSupportedSockAddr
    -- ** Host address
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
#if defined(IPV6_SOCKET_SUPPORT)
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID
#endif
    -- ** Protocol number
    , ProtocolNumber
    , defaultProtocol
    -- ** Port number
    , PortNumber(..)
    -- PortNumber is used non-abstractly in Network.BSD.  ToDo: remove
    -- this use and make the type abstract.

    -- * Address operations

    , HostName
    , ServiceName

#if defined(IPV6_SOCKET_SUPPORT)
    -- ** getaddrinfo
    , AddrInfo(..)

    , AddrInfoFlag(..)
    , addrInfoFlagImplemented

    , defaultHints

    , getAddrInfo

    -- ** getnameinfo
    , NameInfoFlag(..)

    , getNameInfo
#endif

    -- * Socket operations
    , socket
#if defined(DOMAIN_SOCKET_SUPPORT)
    , socketPair
#endif
    , connect
    , bind
    , listen
    , accept
    , getPeerName
    , getSocketName

#if defined(HAVE_STRUCT_UCRED) || defined(HAVE_GETPEEREID)
    -- get the credentials of our domain socket peer.
    , getPeerCred
#if defined(HAVE_GETPEEREID)
    , getPeerEid
#endif
#endif

    , socketPort

    , socketToHandle

    -- ** Sending and receiving data
    , sendBuf
    , recvBuf
    , sendBufTo
    , recvBufFrom

    -- ** Closing
    , close
    , shutdown
    , ShutdownCmd(..)

    -- ** Predicates on sockets
    , isConnected
    , isBound
    , isListening
    , isReadable
    , isWritable

    -- * Socket options
    , SocketOption(..)
    , isSupportedSocketOption
    , getSocketOption
    , setSocketOption

    -- * File descriptor transmission
#ifdef DOMAIN_SOCKET_SUPPORT
    , sendFd
    , recvFd

#endif

    -- * Special constants
    , aNY_PORT
    , iNADDR_ANY
#if defined(IPV6_SOCKET_SUPPORT)
    , iN6ADDR_ANY
#endif
    , sOMAXCONN
    , sOL_SOCKET
#ifdef SCM_RIGHTS
    , sCM_RIGHTS
#endif
    , maxListenQueue

    -- * Initialisation
    , withSocketsDo

    -- * Low level operations
    -- in case you ever want to get at the underlying file descriptor..
    , mkSocket
    , setNonBlockIfNeeded

    -- * Internal

    -- | The following are exported ONLY for use in the BSD module and
    -- should not be used anywhere else.

    , packFamily
    , unpackFamily
    , packSocketType

    -- * Deprecated
    , send
    , sendTo
    , recv
    , recvLen
    , recvFrom
    , inet_addr
    , inet_ntoa
    , htonl
    , ntohl
    , fdSocket
    ) where

import Control.Concurrent.MVar
import Control.Monad (liftM)
import Data.Bits
import Data.Functor
import Data.List (foldl')
import Data.Typeable
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..), CSize(..), CChar)
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Utils ( maybeWith, with )
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import System.IO
import System.IO.Error

# if defined(mingw32_HOST_OS)
import qualified Control.Exception as E
import GHC.Conc (asyncDoProc)
import GHC.IO.FD (FD(..), readRawBufferPtr, writeRawBufferPtr)
import Foreign (FunPtr)
# endif
# if defined(darwin_HOST_OS)
import Data.List (delete)
# endif
import qualified GHC.IO.Device
import GHC.IO.Handle.FD
import GHC.IO.Exception
import GHC.IO

import Network.Socket.Buffer
import Network.Socket.Close
import Network.Socket.Constant
import Network.Socket.Internal
import Network.Socket.Name
import Network.Socket.Options
import Network.Socket.String
import Network.Socket.Syscall
import Network.Socket.Types

import Prelude -- Silence AMP warnings

-- | Either a host name e.g., @\"haskell.org\"@ or a numeric host
-- address string consisting of a dotted decimal IPv4 address or an
-- IPv6 address e.g., @\"192.168.0.1\"@.
type HostName       = String
type ServiceName    = String

{-# DEPRECATED fdSocket "Use sockFd intead" #-}
fdSocket :: Socket -> CInt
fdSocket = sockFd

-- | This is the default protocol for a given service.
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

-----------------------------------------------------------------------------
-- SockAddr

instance Show SockAddr where
#if defined(DOMAIN_SOCKET_SUPPORT)
  showsPrec _ (SockAddrUnix str) = showString str
#else
  showsPrec _ SockAddrUnix{} = error "showsPrec: not supported"
#endif
  showsPrec _ (SockAddrInet port ha)
   = showString (unsafePerformIO (inet_ntoa ha))
   . showString ":"
   . shows port
#if defined(IPV6_SOCKET_SUPPORT)
  showsPrec _ addr@(SockAddrInet6 port _ _ _)
   = showChar '['
   . showString (unsafePerformIO $
                 fst `liftM` getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return)
   . showString "]:"
   . shows port
#else
  showsPrec _ addr@SockAddrInet6{} = error "showsPrec: not supported"
#endif

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

socketPort :: Socket            -- Connected & Bound Socket
           -> IO PortNumber     -- Port Number of Socket
socketPort sock@(MkSocket _ AF_INET _ _ _) = do
    (SockAddrInet port _) <- getSocketName sock
    return port
#if defined(IPV6_SOCKET_SUPPORT)
socketPort sock@(MkSocket _ AF_INET6 _ _ _) = do
    (SockAddrInet6 port _ _ _) <- getSocketName sock
    return port
#endif
socketPort (MkSocket _ family _ _ _) =
    ioError $ userError $
      "Network.Socket.socketPort: address family '" ++ show family ++
      "' not supported."


-- -----------------------------------------------------------------------------
-- Internet address manipulation routines:

inet_addr :: String -> IO HostAddress
inet_addr ipstr = withSocketsDo $ do
   withCString ipstr $ \str -> do
   had <- c_inet_addr str
   if had == maxBound
    then ioError $ userError $
      "Network.Socket.inet_addr: Malformed address: " ++ ipstr
    else return had  -- network byte order

inet_ntoa :: HostAddress -> IO String
inet_ntoa haddr = withSocketsDo $ do
  pstr <- c_inet_ntoa haddr
  peekCString pstr

-- | Turns a Socket into an 'Handle'. By default, the new handle is
-- unbuffered. Use 'System.IO.hSetBuffering' to change the buffering.
--
-- Note that since a 'Handle' is automatically closed by a finalizer
-- when it is no longer referenced, you should avoid doing any more
-- operations on the 'Socket' after calling 'socketToHandle'.  To
-- close the 'Socket' after 'socketToHandle', call 'System.IO.hClose'
-- on the 'Handle'.

socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle s@(MkSocket fd _ _ _ socketStatus) mode = do
 modifyMVar socketStatus $ \ status ->
    if status == ConvertedToHandle
        then ioError (userError ("socketToHandle: already a Handle"))
        else do
    h <- fdToHandle' (fromIntegral fd) (Just GHC.IO.Device.Stream) True (show s) mode True{-bin-}
    hSetBuffering h NoBuffering
    return (ConvertedToHandle, h)

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

packBits :: (Eq a, Num b, Bits b) => [(a, b)] -> [a] -> b

packBits mapping xs = foldl' pack 0 mapping
    where pack acc (k, v) | k `elem` xs = acc .|. v
                          | otherwise   = acc

-- | Unpack a bitmask into a list of values.

unpackBits :: (Num b, Bits b) => [(a, b)] -> b -> [a]

-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

-----------------------------------------------------------------------------
-- Address and service lookups

#if defined(IPV6_SOCKET_SUPPORT)

-- | Flags that control the querying behaviour of 'getAddrInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-25>
data AddrInfoFlag =
    -- | The list of returned 'AddrInfo' values will
    --   only contain IPv4 addresses if the local system has at least
    --   one IPv4 interface configured, and likewise for IPv6.
    --   (Only some platforms support this.)
      AI_ADDRCONFIG
    -- | If 'AI_ALL' is specified, return all matching IPv6 and
    --   IPv4 addresses.  Otherwise, this flag has no effect.
    --   (Only some platforms support this.)
    | AI_ALL
    -- | The 'addrCanonName' field of the first returned
    --   'AddrInfo' will contain the "canonical name" of the host.
    | AI_CANONNAME
    -- | The 'HostName' argument /must/ be a numeric
    --   address in string form, and network name lookups will not be
    --   attempted.
    | AI_NUMERICHOST
    -- | The 'ServiceName' argument /must/ be a port
    --   number in string form, and service name lookups will not be
    --   attempted. (Only some platforms support this.)
    | AI_NUMERICSERV
    -- | If no 'HostName' value is provided, the network
    --   address in each 'SockAddr'
    --   will be left as a "wild card", i.e. as either 'iNADDR_ANY'
    --   or 'iN6ADDR_ANY'.  This is useful for server applications that
    --   will accept connections from any client.
    | AI_PASSIVE
    -- | If an IPv6 lookup is performed, and no IPv6
    --   addresses are found, IPv6-mapped IPv4 addresses will be
    --   returned. (Only some platforms support this.)
    | AI_V4MAPPED
    deriving (Eq, Read, Show, Typeable)

aiFlagMapping :: [(AddrInfoFlag, CInt)]

aiFlagMapping =
    [
#if HAVE_DECL_AI_ADDRCONFIG
     (AI_ADDRCONFIG, #const AI_ADDRCONFIG),
#else
     (AI_ADDRCONFIG, 0),
#endif
#if HAVE_DECL_AI_ALL
     (AI_ALL, #const AI_ALL),
#else
     (AI_ALL, 0),
#endif
     (AI_CANONNAME, #const AI_CANONNAME),
     (AI_NUMERICHOST, #const AI_NUMERICHOST),
#if HAVE_DECL_AI_NUMERICSERV
     (AI_NUMERICSERV, #const AI_NUMERICSERV),
#else
     (AI_NUMERICSERV, 0),
#endif
     (AI_PASSIVE, #const AI_PASSIVE),
#if HAVE_DECL_AI_V4MAPPED
     (AI_V4MAPPED, #const AI_V4MAPPED)
#else
     (AI_V4MAPPED, 0)
#endif
    ]

-- | Indicate whether the given 'AddrInfoFlag' will have any effect on
-- this system.
addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented f = packBits aiFlagMapping [f] /= 0

data AddrInfo =
    AddrInfo {
        addrFlags :: [AddrInfoFlag],
        addrFamily :: Family,
        addrSocketType :: SocketType,
        addrProtocol :: ProtocolNumber,
        addrAddress :: SockAddr,
        addrCanonName :: Maybe String
        }
    deriving (Eq, Show, Typeable)

instance Storable AddrInfo where
    sizeOf    _ = #const sizeof(struct addrinfo)
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        ai_flags <- (#peek struct addrinfo, ai_flags) p
        ai_family <- (#peek struct addrinfo, ai_family) p
        ai_socktype <- (#peek struct addrinfo, ai_socktype) p
        ai_protocol <- (#peek struct addrinfo, ai_protocol) p
        ai_addr <- (#peek struct addrinfo, ai_addr) p >>= peekSockAddr
        ai_canonname_ptr <- (#peek struct addrinfo, ai_canonname) p

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else liftM Just $ peekCString ai_canonname_ptr

        socktype <- unpackSocketType' "AddrInfo.peek" ai_socktype
        return (AddrInfo
                {
                 addrFlags = unpackBits aiFlagMapping ai_flags,
                 addrFamily = unpackFamily ai_family,
                 addrSocketType = socktype,
                 addrProtocol = ai_protocol,
                 addrAddress = ai_addr,
                 addrCanonName = ai_canonname
                })

    poke p (AddrInfo flags family socketType protocol _ _) = do
        c_stype <- packSocketTypeOrThrow "AddrInfo.poke" socketType

        (#poke struct addrinfo, ai_flags) p (packBits aiFlagMapping flags)
        (#poke struct addrinfo, ai_family) p (packFamily family)
        (#poke struct addrinfo, ai_socktype) p c_stype
        (#poke struct addrinfo, ai_protocol) p protocol

        -- stuff below is probably not needed, but let's zero it for safety

        (#poke struct addrinfo, ai_addrlen) p (0::CSize)
        (#poke struct addrinfo, ai_addr) p nullPtr
        (#poke struct addrinfo, ai_canonname) p nullPtr
        (#poke struct addrinfo, ai_next) p nullPtr

-- | Flags that control the querying behaviour of 'getNameInfo'.
--   For more information, see <https://tools.ietf.org/html/rfc3493#page-30>
data NameInfoFlag =
    -- | Resolve a datagram-based service name.  This is
    --   required only for the few protocols that have different port
    --   numbers for their datagram-based versions than for their
    --   stream-based versions.
      NI_DGRAM
    -- | If the hostname cannot be looked up, an IO error is thrown.
    | NI_NAMEREQD
    -- | If a host is local, return only the hostname part of the FQDN.
    | NI_NOFQDN
    -- | The name of the host is not looked up.
    --   Instead, a numeric representation of the host's
    --   address is returned.  For an IPv4 address, this will be a
    --   dotted-quad string.  For IPv6, it will be colon-separated
    --   hexadecimal.
    | NI_NUMERICHOST
    -- | The name of the service is not
    --   looked up.  Instead, a numeric representation of the
    --   service is returned.
    | NI_NUMERICSERV
    deriving (Eq, Read, Show, Typeable)

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, #const NI_DGRAM),
                 (NI_NAMEREQD, #const NI_NAMEREQD),
                 (NI_NOFQDN, #const NI_NOFQDN),
                 (NI_NUMERICHOST, #const NI_NUMERICHOST),
                 (NI_NUMERICSERV, #const NI_NUMERICSERV)]

-- | Default hints for address lookup with 'getAddrInfo'.  The values
-- of the 'addrAddress' and 'addrCanonName' fields are 'undefined',
-- and are never inspected by 'getAddrInfo'.
--
-- >>> addrFlags defaultHints
-- []
-- >>> addrFamily defaultHints
-- AF_UNSPEC
-- >>> addrSocketType defaultHints
-- NoSocketType
-- >>> addrProtocol defaultHints
-- 0

defaultHints :: AddrInfo
defaultHints = AddrInfo {
                         addrFlags = [],
                         addrFamily = AF_UNSPEC,
                         addrSocketType = NoSocketType,
                         addrProtocol = defaultProtocol,
                         addrAddress = undefined,
                         addrCanonName = undefined
                        }

-- | Resolve a host or service name to one or more addresses.
-- The 'AddrInfo' values that this function returns contain 'SockAddr'
-- values that you can pass directly to 'connect' or
-- 'bind'.
--
-- This function is protocol independent.  It can return both IPv4 and
-- IPv6 address information.
--
-- The 'AddrInfo' argument specifies the preferred query behaviour,
-- socket options, or protocol.  You can override these conveniently
-- using Haskell's record update syntax on 'defaultHints', for example
-- as follows:
--
-- >>> let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }
--
-- You must provide a 'Just' value for at least one of the 'HostName'
-- or 'ServiceName' arguments.  'HostName' can be either a numeric
-- network address (dotted quad for IPv4, colon-separated hex for
-- IPv6) or a hostname.  In the latter case, its addresses will be
-- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
-- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
-- a hint, network addresses in the result will contain the address of
-- the loopback interface.
--
-- If the query fails, this function throws an IO exception instead of
-- returning an empty list.  Otherwise, it returns a non-empty list
-- of 'AddrInfo' values.
--
-- There are several reasons why a query might result in several
-- values.  For example, the queried-for host could be multihomed, or
-- the service might be available via several protocols.
--
-- Note: the order of arguments is slightly different to that defined
-- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
-- to make partial application easier.
--
-- >>> addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "http")
-- >>> addrAddress addr
-- 127.0.0.1:80

getAddrInfo :: Maybe AddrInfo -- ^ preferred socket type or protocol
            -> Maybe HostName -- ^ host name to look up
            -> Maybe ServiceName -- ^ service name to look up
            -> IO [AddrInfo] -- ^ resolved addresses, with "best" first

getAddrInfo hints node service = withSocketsDo $
  maybeWith withCString node $ \c_node ->
    maybeWith withCString service $ \c_service ->
      maybeWith with filteredHints $ \c_hints ->
        alloca $ \ptr_ptr_addrs -> do
          ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
          case ret of
            0 -> do ptr_addrs <- peek ptr_ptr_addrs
                    ais <- followAddrInfo ptr_addrs
                    c_freeaddrinfo ptr_addrs
                    return ais
            _ -> do err <- gai_strerror ret
                    ioError (ioeSetErrorString
                             (mkIOError NoSuchThing "Network.Socket.getAddrInfo" Nothing
                              Nothing) err)
    -- Leaving out the service and using AI_NUMERICSERV causes a
    -- segfault on OS X 10.8.2. This code removes AI_NUMERICSERV
    -- (which has no effect) in that case.
  where
#if defined(darwin_HOST_OS)
    filteredHints = case service of
        Nothing -> fmap (\ h -> h { addrFlags = delete AI_NUMERICSERV (addrFlags h) }) hints
        _       -> hints
#else
    filteredHints = hints
#endif

followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]

followAddrInfo ptr_ai | ptr_ai == nullPtr = return []
                      | otherwise = do
    a <- peek ptr_ai
    as <- (#peek struct addrinfo, ai_next) ptr_ai >>= followAddrInfo
    return (a:as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

gai_strerror :: CInt -> IO String

#ifdef HAVE_GAI_STRERROR
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString
#else
gai_strerror n = ioError $ userError $ "Network.Socket.gai_strerror not supported: " ++ show n
#endif

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True n f = allocaBytes n (f (fromIntegral n))

-- | Resolve an address to a host or service name.
-- This function is protocol independent.
-- The list of 'NameInfoFlag' values controls query behaviour.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- Example:
-- @
--   (hostName, _) <- getNameInfo [] True False myAddress
-- @

getNameInfo :: [NameInfoFlag] -- ^ flags to control lookup behaviour
            -> Bool -- ^ whether to look up a hostname
            -> Bool -- ^ whether to look up a service name
            -> SockAddr -- ^ the address to look up
            -> IO (Maybe HostName, Maybe ServiceName)

getNameInfo flags doHost doService addr = withSocketsDo $
  withCStringIf doHost (#const NI_MAXHOST) $ \c_hostlen c_host ->
    withCStringIf doService (#const NI_MAXSERV) $ \c_servlen c_serv -> do
      withSockAddr addr $ \ptr_addr sz -> do
        ret <- c_getnameinfo ptr_addr (fromIntegral sz) c_host c_hostlen
                             c_serv c_servlen (packBits niFlagMapping flags)
        case ret of
          0 -> do
            let peekIf doIf c_val = if doIf
                                     then liftM Just $ peekCString c_val
                                     else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return (host, serv)
          _ -> do err <- gai_strerror ret
                  ioError (ioeSetErrorString
                           (mkIOError NoSuchThing "Network.Socket.getNameInfo" Nothing
                            Nothing) err)

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt
#endif

-- ---------------------------------------------------------------------------
-- foreign imports from the C library

foreign import ccall unsafe "hsnet_inet_ntoa"
  c_inet_ntoa :: HostAddress -> IO (Ptr CChar)

foreign import CALLCONV unsafe "inet_addr"
  c_inet_addr :: Ptr CChar -> IO HostAddress
