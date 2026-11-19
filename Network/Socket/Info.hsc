{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Info where

import Control.Exception (Exception (..), mask, onException)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (maybeWith, with)
import Foreign.C.Error (Errno (..), errnoToIOError, getErrno)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error (ioeSetErrorString, mkIOError)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Syscall
import Network.Socket.Types

-----------------------------------------------------------------------------

-- | Either a host name e.g., @\"haskell.org\"@ or a numeric host
-- address string consisting of a dotted decimal IPv4 address or an
-- IPv6 address e.g., @\"192.168.0.1\"@.
--
-- An internationalized domain name must be given in its IDNA A-label
-- (Punycode) form, e.g. @\"xn--mnchen-3ya.de\"@, not @\"münchen.de\"@.
-- Whether a name with non-ASCII characters resolves depends on the OS:
-- macOS converts it, but glibc and Windows do not.  Converting a name to
-- A-labels (IDNA2008 or UTS #46) is up to the application, which needs
-- them anyway for TLS SNI and certificates.  'getNameInfo' returns
-- A-labels.
type HostName       = String
-- | Either a service name e.g., @\"http\"@ or a numeric port number.
type ServiceName    = String

-----------------------------------------------------------------------------
-- Address and service lookups

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
    --   will be left as a "wild card".
    --   This is useful for server applications that
    --   will accept connections from any client.
    | AI_PASSIVE
    -- | If an IPv6 lookup is performed, and no IPv6
    --   addresses are found, IPv6-mapped IPv4 addresses will be
    --   returned. (Only some platforms support this.)
    | AI_V4MAPPED
    deriving (Eq, Read, Show)

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

-- | An address which 'getAddrInfo' returns, with what is needed to open
--   a socket for it (@struct addrinfo@).  Given to 'getAddrInfo' as
--   hints, usually 'defaultHints' with some fields changed, it narrows
--   the lookup; 'addrAddress' and 'addrCanonName' are then ignored.
data AddrInfo = AddrInfo {
    -- | Flags: in hints, how to look up; in results, as the OS sets them
    --   (usually those of the hints).
    addrFlags :: [AddrInfoFlag]
    -- | Address family, e.g. 'AF_INET' or 'AF_INET6'; 'AF_UNSPEC' in
    --   hints for any.
  , addrFamily :: Family
    -- | Socket type, e.g. 'Stream'; 'NoSocketType' in hints for any.
  , addrSocketType :: SocketType
    -- | Protocol; 'defaultProtocol' in hints for any.
  , addrProtocol :: ProtocolNumber
    -- | The address.
  , addrAddress :: SockAddr
    -- | The canonical name of the host, in the first result only, and
    --   only with 'AI_CANONNAME' in the hints.
  , addrCanonName :: Maybe String
  } deriving (Eq, Show)

instance Storable AddrInfo where
    sizeOf    ~_ = #const sizeof(struct addrinfo)
    alignment ~_ = alignment (0 :: CInt)

    peek p = do
        ai_flags <- (#peek struct addrinfo, ai_flags) p
        ai_family <- (#peek struct addrinfo, ai_family) p
        ai_socktype <- (#peek struct addrinfo, ai_socktype) p
        ai_protocol <- (#peek struct addrinfo, ai_protocol) p
        ai_addr <- (#peek struct addrinfo, ai_addr) p >>= peekSockAddr
        ai_canonname_ptr <- (#peek struct addrinfo, ai_canonname) p

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else Just <$> peekCString ai_canonname_ptr

        return $ AddrInfo {
            addrFlags = unpackBits aiFlagMapping ai_flags
          , addrFamily = unpackFamily ai_family
          , addrSocketType = unpackSocketType ai_socktype
          , addrProtocol = ai_protocol
          , addrAddress = ai_addr
          , addrCanonName = ai_canonname
          }

    poke p (AddrInfo flags family sockType protocol _ _) = do
        let c_stype = packSocketType sockType

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
    deriving (Eq, Read, Show)

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, #const NI_DGRAM),
                 (NI_NAMEREQD, #const NI_NAMEREQD),
                 (NI_NOFQDN, #const NI_NOFQDN),
                 (NI_NUMERICHOST, #const NI_NUMERICHOST),
                 (NI_NUMERICSERV, #const NI_NUMERICSERV)]

-- | Default hints for address lookup with 'getAddrInfo'.
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
    addrFlags      = []
  , addrFamily     = AF_UNSPEC
  , addrSocketType = NoSocketType
  , addrProtocol   = defaultProtocol
  , addrAddress    = SockAddrInet 0 0
  , addrCanonName  = Nothing
  }

class GetAddrInfo t where
    -----------------------------------------------------------------------------
    -- | Resolve a host or service name to one or more addresses.
    -- The 'AddrInfo' values that this function returns contain 'SockAddr'
    -- values that you can pass directly to 'connect' or
    -- 'bind'.
    --
    -- This function calls @getaddrinfo(3)@, which never successfully returns
    -- with an empty list. If the query fails, 'getAddrInfo' throws an IO
    -- exception.
    --
    -- For backwards-compatibility reasons, a hidden 'GetAddrInfo' class is used
    -- to make the result polymorphic. It only has instances for @[]@ (lists)
    -- and 'NonEmpty'. Use of 'NonEmpty' is recommended.
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
    -- There are several reasons why a query might result in several
    -- values.  For example, the queried-for host could be multihomed, or
    -- the service might be available via several protocols.
    --
    -- Note: the order of arguments is slightly different to that defined
    -- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
    -- to make partial application easier.
    --
    -- >>> import qualified Data.List.NonEmpty as NE
    -- >>> addr <- NE.head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "http")
    -- >>> addrAddress addr
    -- 127.0.0.1:80
    --
    -- Polymorphic version: @since 3.2.3.0
    getAddrInfo
        :: Maybe AddrInfo -- ^ preferred socket type or protocol
        -> Maybe HostName -- ^ host name to look up
        -> Maybe ServiceName -- ^ service name to look up
        -> IO (t AddrInfo) -- ^ resolved addresses, with "best" first

instance GetAddrInfo [] where
    getAddrInfo = getAddrInfoList

instance GetAddrInfo NE.NonEmpty where
    getAddrInfo = getAddrInfoNE

getAddrInfoNE
    :: Maybe AddrInfo -- ^ preferred socket type or protocol
    -> Maybe HostName -- ^ host name to look up
    -> Maybe ServiceName -- ^ service name to look up
    -> IO (NonEmpty AddrInfo) -- ^ resolved addresses, with "best" first
getAddrInfoNE hints node service =
    getaddrinfo hints node service >>= either (throwGaiError message) return
  where
    message = concat [
        "Network.Socket.getAddrInfo (called with preferred socket type/protocol: "
      , maybe "Nothing" show hints
      , ", host name: "
      , maybe "Nothing" show node
      , ", service name: "
      , maybe "Nothing" show service
      , ")"
      ]

-- | Resolve a host or service name to one or more addresses, as
--   'getAddrInfo' does, but returning a failure as the @EAI_*@ code
--   that getaddrinfo(3) reported rather than throwing it.
--
--   'withSocketsDo' can still throw on Windows, and so can a
--   getaddrinfo(3) which succeeds with no address, which POSIX rules out,
--   or with an address of a family which 'SockAddr' does not support.
--
--   @since 4.0.0.0
getaddrinfo
    :: Maybe AddrInfo -- ^ preferred socket type or protocol
    -> Maybe HostName -- ^ host name to look up
    -> Maybe ServiceName -- ^ service name to look up
    -> IO (Either GaiError (NonEmpty AddrInfo)) -- ^ resolved addresses, with "best" first
getaddrinfo hints node service = alloc resolve
  where
    alloc body = withSocketsDo $ maybeWith withCString node $ \c_node ->
        maybeWith withCString service                       $ \c_service ->
            maybeWith with filteredHints                    $ \c_hints ->
                  alloca                                    $ \ptr_ptr_addrs ->
                      body c_node c_service c_hints ptr_ptr_addrs
    resolve c_node c_service c_hints ptr_ptr_addrs = mask $ \release -> do
        ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
        if ret == 0 then do
            ptr_addrs <- peek ptr_ptr_addrs
            ais       <- release (followAddrInfo ptr_addrs) `onException` c_freeaddrinfo ptr_addrs
            c_freeaddrinfo ptr_addrs
            return $ Right ais
          else Left <$> toGaiError ret
#if defined(darwin_HOST_OS)
    -- Leaving out the service and using AI_NUMERICSERV causes a
    -- segfault on OS X 10.8.2. This code removes AI_NUMERICSERV
    -- (which has no effect) in that case.
    toHints h = h { addrFlags = delete AI_NUMERICSERV (addrFlags h) }
    filteredHints = case service of
        Nothing -> toHints <$> hints
        _       -> hints
#else
    filteredHints = hints
#endif

getAddrInfoList
    :: Maybe AddrInfo
    -> Maybe HostName
    -> Maybe ServiceName
    -> IO [AddrInfo]
getAddrInfoList hints node service =
    -- getAddrInfo never returns an empty list.
    NE.toList <$> getAddrInfoNE hints node service

followAddrInfo :: Ptr AddrInfo -> IO (NonEmpty AddrInfo)
followAddrInfo ptr_ai
    -- POSIX requires that getaddrinfo(3) returns at least one addrinfo.
    -- See: http://pubs.opengroup.org/onlinepubs/9699919799/functions/getaddrinfo.html
    | ptr_ai == nullPtr = ioError $ mkIOError NoSuchThing "getaddrinfo must return at least one addrinfo" Nothing Nothing
    | otherwise = do
        a <- peek ptr_ai
        ptr <- (# peek struct addrinfo, ai_next) ptr_ai
        (a :|) <$> go ptr
  where
    go :: Ptr AddrInfo -> IO [AddrInfo]
    go ptr
      | ptr == nullPtr = return []
      | otherwise = do
            a' <- peek ptr
            ptr' <- (# peek struct addrinfo, ai_next) ptr
            as' <- go ptr'
            return (a':as')

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

-- | An @EAI_*@ code, as getaddrinfo(3) and getnameinfo(3) return it.
--   Only network makes one: match it with the patterns below, and with
--   'GaiErrorCode' or @_@ for codes they do not name.  'EAI_SYSTEM'
--   carries the errno it stands for.  On Windows, the codes are Winsock codes.
--
--   @since 4.0.0.0
data GaiError = GaiCode CInt | GaiSystem Errno
    deriving Eq

-- Codes an OS lacks (only Windows lacks any) get values that no OS
-- returns, each its own, so that they match nothing real.
#ifndef EAI_ADDRFAMILY
# define EAI_ADDRFAMILY (-2147483647)
#endif
#ifndef EAI_NODATA
# define EAI_NODATA (-2147483646)
#endif
#ifndef EAI_OVERFLOW
# define EAI_OVERFLOW (-2147483645)
#endif
#ifndef EAI_SYSTEM
# define EAI_SYSTEM (-2147483644)
#endif

-- | POSIX codes.
pattern EAI_AGAIN, EAI_BADFLAGS, EAI_FAIL, EAI_FAMILY, EAI_MEMORY,
        EAI_NONAME, EAI_OVERFLOW, EAI_SERVICE, EAI_SOCKTYPE :: GaiError
pattern EAI_AGAIN    = GaiCode (#const EAI_AGAIN)
pattern EAI_BADFLAGS = GaiCode (#const EAI_BADFLAGS)
pattern EAI_FAIL     = GaiCode (#const EAI_FAIL)
pattern EAI_FAMILY   = GaiCode (#const EAI_FAMILY)
pattern EAI_MEMORY   = GaiCode (#const EAI_MEMORY)
pattern EAI_NONAME   = GaiCode (#const EAI_NONAME)
-- | Not on Windows.
pattern EAI_OVERFLOW = GaiCode (#const EAI_OVERFLOW)
pattern EAI_SERVICE  = GaiCode (#const EAI_SERVICE)
pattern EAI_SOCKTYPE = GaiCode (#const EAI_SOCKTYPE)

-- | Not POSIX, but returned by glibc and others; 'EAI_ADDRFAMILY' is
--   not on Windows.
pattern EAI_ADDRFAMILY, EAI_NODATA :: GaiError
pattern EAI_ADDRFAMILY = GaiCode (#const EAI_ADDRFAMILY)
pattern EAI_NODATA     = GaiCode (#const EAI_NODATA)

-- | The system call failed with this errno.  Not on Windows.
pattern EAI_SYSTEM :: Errno -> GaiError
pattern EAI_SYSTEM errno = GaiSystem errno

-- | Any code but 'EAI_SYSTEM', by its number, e.g. one the patterns
--   above do not name.  It only matches: make codes with the patterns.
--
--   @since 4.0.0.0
pattern GaiErrorCode :: CInt -> GaiError
pattern GaiErrorCode rc <- GaiCode rc

-- | The @EAI_*@ code itself.
--
--   @since 4.0.0.0
gaiErrorCode :: GaiError -> CInt
gaiErrorCode (GaiCode rc)  = rc
gaiErrorCode (GaiSystem _) = #const EAI_SYSTEM

-- The code that the C call has just returned.  errno is read at once:
-- GHC keeps it for the Haskell thread across the safe call.
toGaiError :: CInt -> IO GaiError
toGaiError rc
    | rc == #{const EAI_SYSTEM} = GaiSystem <$> getErrno
    | otherwise                 = return $ GaiCode rc

-- The names and, in glibc's English, the meanings of the named codes.
gaiErrorTable :: [(GaiError, (String, String))]
gaiErrorTable =
    [ (EAI_ADDRFAMILY, ("EAI_ADDRFAMILY", "Address family for hostname not supported"))
    , (EAI_AGAIN,      ("EAI_AGAIN",      "Temporary failure in name resolution"))
    , (EAI_BADFLAGS,   ("EAI_BADFLAGS",   "Bad value for ai_flags"))
    , (EAI_FAIL,       ("EAI_FAIL",       "Non-recoverable failure in name resolution"))
    , (EAI_FAMILY,     ("EAI_FAMILY",     "ai_family not supported"))
    , (EAI_MEMORY,     ("EAI_MEMORY",     "Memory allocation failure"))
    , (EAI_NODATA,     ("EAI_NODATA",     "No address associated with hostname"))
    , (EAI_NONAME,     ("EAI_NONAME",     "Name or service not known"))
    , (EAI_OVERFLOW,   ("EAI_OVERFLOW",   "Argument buffer overflow"))
    , (EAI_SERVICE,    ("EAI_SERVICE",    "Servname not supported for ai_socktype"))
    , (EAI_SOCKTYPE,   ("EAI_SOCKTYPE",   "ai_socktype not supported"))
    ]

instance Show GaiError where
    showsPrec d (GaiSystem (Errno n)) = showParen (d > 10) $
        showString "EAI_SYSTEM (Errno " . shows n . showChar ')'
    showsPrec d e@(GaiCode rc) = case lookup e gaiErrorTable of
        Just (name, _) -> showString name
        Nothing        -> showParen (d > 10) $
            showString "GaiErrorCode " . showsPrec 11 rc

instance Exception GaiError where
    displayException = gaiErrorString

-- | What the code means, for people.  The named codes are described in
--   English, whatever the locale; 'EAI_SYSTEM' by its errno, as base
--   describes errnos; other codes as the OS describes them.
--
--   @since 4.0.0.0
gaiErrorString :: GaiError -> String
gaiErrorString (GaiSystem errno) =
    ioe_description $ errnoToIOError "" errno Nothing Nothing
gaiErrorString e@(GaiCode rc) = case lookup e gaiErrorTable of
    Just (_, str) -> str
#if defined(mingw32_HOST_OS)
    -- Winsock codes; the table holds constant strings.
    Nothing -> unsafePerformIO $ winsockErrorString rc
#elif defined(HAVE_GAI_STRERROR)
    -- Codes only some OSes have, such as EAI_BADHINTS on the BSDs.
    Nothing -> unsafePerformIO $ c_gai_strerror rc >>= peekCString

foreign import ccall unsafe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString
#else
    Nothing -> "Unknown error " ++ show rc
#endif

-- | Throwing a 'GaiError' as an 'IOException', as 'getAddrInfo' and
--   'getNameInfo' do: 'EAI_SYSTEM' as its errno, 'EAI_AGAIN' as
--   'ResourceExhausted' (as @EAGAIN@ is), and others as 'NoSuchThing'.
--   On Windows the code is kept in 'ioe_errno', as for other socket
--   errors.
throwGaiError :: String -> GaiError -> IO a
throwGaiError loc (GaiSystem errno) = ioError $ errnoToIOError loc errno Nothing Nothing
throwGaiError loc e = ioError $ ioeSetErrorString ioe' (gaiErrorString e)
  where
    typ = case e of
        EAI_AGAIN -> ResourceExhausted
        _         -> NoSuchThing
    ioe = mkIOError typ loc Nothing Nothing
#if defined(mingw32_HOST_OS)
    ioe' = ioe{ioe_errno = Just (gaiErrorCode e)}
#else
    ioe' = ioe
#endif

-----------------------------------------------------------------------------

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True  n f = allocaBytes n (f (fromIntegral n))

-- | Resolve an address to a host or service name.
-- This function is protocol independent.
-- The list of 'NameInfoFlag' values controls query behaviour.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- >>> addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "http")
-- >>> getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True $ addrAddress addr
-- (Just "127.0.0.1",Just "80")
{-
-- >>> getNameInfo [] True True $ addrAddress addr
-- (Just "localhost",Just "http")
-}
getNameInfo
    :: [NameInfoFlag] -- ^ flags to control lookup behaviour
    -> Bool -- ^ whether to look up a hostname
    -> Bool -- ^ whether to look up a service name
    -> SockAddr -- ^ the address to look up
    -> IO (Maybe HostName, Maybe ServiceName)
getNameInfo flags doHost doService addr =
    nameInfo flags doHost doService addr >>= either (throwGaiError message) return
  where
    message = concat [
        "Network.Socket.getNameInfo (called with flags: "
      , show flags
      , ", hostname lookup: "
      , show doHost
      , ", service name lookup: "
      , show doService
      , ", socket address: "
      , show addr
      , ")"
      ]

-- | What 'getnameinfo' looks up, and so what it returns.
--
--   @since 4.0.0.0
data NameInfoQuery a where
    QueryHost           :: NameInfoQuery HostName
    QueryService        :: NameInfoQuery ServiceName
    QueryHostAndService :: NameInfoQuery (HostName, ServiceName)

-- | Resolve an address to a host or service name, as 'getNameInfo'
--   does, but returning a failure as the @EAI_*@ code that getnameinfo(3)
--   reported rather than throwing it.  Unlike getnameinfo(3), it takes
--   what to look up as a 'NameInfoQuery'.  Flags which do not apply to
--   the query, such as 'NI_NAMEREQD' with 'QueryService', are ignored.
--
--   'withSocketsDo' can still throw on Windows, and so can a
--   'SockAddrUnix' whose path is too long.
--
--   @since 4.0.0.0
getnameinfo
    :: [NameInfoFlag] -- ^ flags to control lookup behaviour
    -> NameInfoQuery a -- ^ what to look up
    -> SockAddr -- ^ the address to look up
    -> IO (Either GaiError a)
getnameinfo flags query addr = case query of
    QueryHost           -> fmap (fromJust . fst) <$> nameInfo flags True False addr
    QueryService        -> fmap (fromJust . snd) <$> nameInfo flags False True addr
    QueryHostAndService ->
        fmap (\(h, sv) -> (fromJust h, fromJust sv)) <$> nameInfo flags True True addr

-- A result is 'Just' exactly when it was asked for.
nameInfo
    :: [NameInfoFlag] -> Bool -> Bool -> SockAddr
    -> IO (Either GaiError (Maybe HostName, Maybe ServiceName))
nameInfo flags doHost doService addr = alloc lookupName
  where
    alloc body = withSocketsDo $
        withCStringIf doHost (# const NI_MAXHOST)        $ \c_hostlen c_host ->
            withCStringIf doService (# const NI_MAXSERV) $ \c_servlen c_serv ->
                withSockAddr addr                        $ \ptr_addr sz ->
                  body c_hostlen c_host c_servlen c_serv ptr_addr sz
    lookupName c_hostlen c_host c_servlen c_serv ptr_addr sz = do
        ret <- c_getnameinfo ptr_addr
                             (fromIntegral sz)
                             c_host
                             c_hostlen
                             c_serv
                             c_servlen
                             (packBits niFlagMapping flags)
        if ret == 0 then do
            let peekIf doIf c_val =
                    if doIf then Just <$> peekCString c_val else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return $ Right (host, serv)
          else Left <$> toGaiError ret

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

-----------------------------------------------------------------------------

packBits :: (Eq a, Num b, Bits b) => [(a, b)] -> [a] -> b
packBits mapping xs = foldl' pack 0 mapping
  where
    pack acc (k, v) | k `elem` xs = acc .|. v
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

-- | A utility function to open a socket with `AddrInfo`.
-- This is a just wrapper for the following code:
--
-- > \addr -> socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
