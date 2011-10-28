{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances,
             ForeignFunctionInterface, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.Internal
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- A module containing semi-public 'Network.Socket' internals.
-- Modules which extend the 'Network.Socket' module will need to use
-- this module while ideally most users will be able to make do with
-- the public interface.
--
-----------------------------------------------------------------------------

#include "HsNet.h"

-- NOTE: ##, we want this interpreted when compiling the .hs, not by hsc2hs.
##include "Typeable.h"

module Network.Socket.Internal
    (
    -- * Socket addresses
      HostAddress
#if defined(IPV6_SOCKET_SUPPORT)
    , HostAddress6
    , FlowInfo
    , ScopeID
#endif
    , PortNumber(..)
    , SockAddr(..)

    , peekSockAddr
    , pokeSockAddr
    , sizeOfSockAddr
    , sizeOfSockAddrByFamily
    , withSockAddr
    , withNewSockAddr

    -- * Protocol families
    , Family(..)
    , packFamily
    , unpackFamily

    -- * Socket error functions
#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
    , c_getLastError
#endif
    , throwSocketError

    -- * Guards for socket operations that may fail
    , throwSocketErrorIfMinus1_
    , throwSocketErrorIfMinus1Retry
    , throwSocketErrorIfMinus1Retry_
    , throwSocketErrorIfMinus1RetryMayBlock

    -- * Initialization
    , withSocketsDo

    -- * Low-level helpers
    , zeroMemory
    ) where

import Control.Monad (liftM)
import Data.Bits ( (.|.), shiftL, shiftR )
import Data.Ratio ((%))
import Data.Word ( Word8, Word16, Word32 )
import Data.Typeable (Typeable)
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1Retry,
                        throwErrnoIfMinus1RetryMayBlock, throwErrnoIfMinus1_)
import Foreign.C.String ( castCharToCChar, peekCString )
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types ( CInt(..), CSize(..) )
#else
import Foreign.C.Types ( CInt, CSize )
#endif
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Marshal.Array ( pokeArray, pokeArray0 )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(..) )

#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
import Control.Exception ( finally )
#  if __GLASGOW_HASKELL__
import GHC.IOBase ( IOErrorType(..) )
#  endif
import Foreign.C.Types ( CChar )
import System.IO.Error ( ioeSetErrorString, mkIOError )
#endif

------------------------------------------------------------------------

-- | An IPv4 address.
type HostAddress = Word32

#if defined(IPV6_SOCKET_SUPPORT)
-- | An IPv6 address.
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = (#offset struct in6_addr, s6_addr)

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i = do
    let i' = i * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        w `sl` n = fromIntegral w `shiftL` n
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i a = do
    let i' = i * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        w `sr` n = fromIntegral (w `shiftR` n) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

instance Storable HostAddress6 where
    sizeOf _    = (#const sizeof(struct in6_addr))
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return (a, b, c, d)

    poke p (a, b, c, d) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d
#endif

------------------------------------------------------------------------
-- Port Numbers
--
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.
--

-- | The port number of a socket.  Construct a 'PortNumber' using
-- 'fromIntegral'.
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable)

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
                  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `liftM` peek (castPtr p)

------------------------------------------------------------------------
-- Socket addresses

-- The scheme used for addressing sockets is somewhat quirky. The
-- calls in the BSD socket API that need to know the socket address
-- all operate in terms of struct sockaddr, a `virtual' type of
-- socket address.

-- The Internet family of sockets are addressed as struct sockaddr_in,
-- so when calling functions that operate on struct sockaddr, we have
-- to type cast the Internet socket address into a struct sockaddr.
-- Instances of the structure for different families might *not* be
-- the same size. Same casting is required of other families of
-- sockets such as Xerox NS. Similarly for Unix domain sockets.

-- To represent these socket addresses in Haskell-land, we do what BSD
-- didn't do, and use a union/algebraic type for the different
-- families. Currently only Unix domain sockets and the Internet
-- families are supported.

#if defined(IPV6_SOCKET_SUPPORT)
-- | IPv6 flow information.
type FlowInfo = Word32

-- | An IPv6 scope ID.
type ScopeID = Word32
#endif

data SockAddr       -- C Names
  = SockAddrInet
    PortNumber  -- sin_port  (network byte order)
    HostAddress -- sin_addr  (ditto)
#if defined(IPV6_SOCKET_SUPPORT)
  | SockAddrInet6
        PortNumber      -- sin6_port (network byte order)
        FlowInfo        -- sin6_flowinfo (ditto)
        HostAddress6    -- sin6_addr (ditto)
        ScopeID         -- sin6_scope_id (ditto)
#endif
#if defined(DOMAIN_SOCKET_SUPPORT)
  | SockAddrUnix
        String          -- sun_path
#endif
  | SockAddrRaw
        Family          -- socket family
        [Word8]         -- raw bytes
  deriving (Eq, Typeable)

#if defined(WITH_WINSOCK) || defined(cygwin32_HOST_OS)
type CSaFamily = (#type unsigned short)
#elif defined(darwin_HOST_OS)
type CSaFamily = (#type u_char)
#else
type CSaFamily = (#type sa_family_t)
#endif

-- | Computes the storage requirements (in bytes) of the given
-- 'SockAddr'.  This function differs from 'Foreign.Storable.sizeOf'
-- in that the value of the argument /is/ used.
sizeOfSockAddr :: SockAddr -> Int
#if defined(DOMAIN_SOCKET_SUPPORT)
sizeOfSockAddr (SockAddrUnix path) =
    case path of
        '\0':_ -> (#const sizeof(sa_family_t)) + length path
        _      -> #const sizeof(struct sockaddr_un)
#endif
sizeOfSockAddr (SockAddrInet _ _) = #const sizeof(struct sockaddr_in)
#if defined(IPV6_SOCKET_SUPPORT)
sizeOfSockAddr (SockAddrInet6 _ _ _ _) = #const sizeof(struct sockaddr_in6)
#endif
sizeOfSockAddr (SockAddrRaw _ bytes) = (#const sizeof(sa_family_t)) + length bytes

-- | Computes the storage requirements (in bytes) required for a
-- 'SockAddr' with the given 'Family'.
sizeOfSockAddrByFamily :: Family -> Int
#if defined(DOMAIN_SOCKET_SUPPORT)
sizeOfSockAddrByFamily AF_UNIX  = #const sizeof(struct sockaddr_un)
#endif
#if defined(IPV6_SOCKET_SUPPORT)
sizeOfSockAddrByFamily AF_INET6 = #const sizeof(struct sockaddr_in6)
#endif
sizeOfSockAddrByFamily AF_INET  = #const sizeof(struct sockaddr_in)

-- | Use a 'SockAddr' with a function requiring a pointer to a
-- 'SockAddr' and the length of that 'SockAddr'.
withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
    let sz = sizeOfSockAddr addr
    allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

-- | Create a new 'SockAddr' for use with a function requiring a
-- pointer to a 'SockAddr' and the length of that 'SockAddr'.
withNewSockAddr :: Family -> (Ptr SockAddr -> Int -> IO a) -> IO a
withNewSockAddr family f = do
    let sz = sizeOfSockAddrByFamily family
    allocaBytes sz $ \ptr -> f ptr sz

-- We can't write an instance of 'Storable' for 'SockAddr' because
-- @sockaddr@ is a sum type of variable size but
-- 'Foreign.Storable.sizeOf' is required to be constant.

-- Note that on Darwin, the sockaddr structure must be zeroed before
-- use.

-- | Write the given 'SockAddr' to the given memory location.
pokeSockAddr :: Ptr a -> SockAddr -> IO ()
#if defined(DOMAIN_SOCKET_SUPPORT)
pokeSockAddr p (SockAddrUnix path) = do
#if defined(darwin_TARGET_OS)
    zeroMemory p (#const sizeof(struct sockaddr_un))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_un, sun_len) p ((#const sizeof(struct sockaddr_un)) :: Word8)
#endif
    (#poke struct sockaddr_un, sun_family) p ((#const AF_UNIX) :: CSaFamily)
    let pathC = map castCharToCChar path
        poker = case path of ('\0':_) -> pokeArray; _ -> pokeArray0 0
    poker ((#ptr struct sockaddr_un, sun_path) p) pathC
#endif
pokeSockAddr p (SockAddrInet (PortNum port) addr) = do
#if defined(darwin_TARGET_OS)
    zeroMemory p (#const sizeof(struct sockaddr_in))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in, sin_len) p ((#const sizeof(struct sockaddr_in)) :: Word8)
#endif
    (#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
    (#poke struct sockaddr_in, sin_port) p port
    (#poke struct sockaddr_in, sin_addr) p addr
#if defined(IPV6_SOCKET_SUPPORT)
pokeSockAddr p (SockAddrInet6 (PortNum port) flow addr scope) = do
#if defined(darwin_TARGET_OS)
    zeroMemory p (#const sizeof(struct sockaddr_in6))
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in6, sin6_len) p ((#const sizeof(struct sockaddr_in6)) :: Word8)
#endif
    (#poke struct sockaddr_in6, sin6_family) p ((#const AF_INET6) :: CSaFamily)
    (#poke struct sockaddr_in6, sin6_port) p port
    (#poke struct sockaddr_in6, sin6_flowinfo) p flow
    (#poke struct sockaddr_in6, sin6_addr) p addr
    (#poke struct sockaddr_in6, sin6_scope_id) p scope
#endif
pokeSockAddr p _sa@(SockAddrRaw family bytes) = do
#if defined(darwin_TARGET_OS)
    zeroMemory p (sizeOfSockAddr _sa)
#endif
    (#poke struct sockaddr, sa_family) p (fromIntegral (packFamily family) :: CSaFamily)
    pokeArray ((#ptr struct sockaddr, sa_data) p) bytes

-- | Read a 'SockAddr' from the given memory location.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
  family <- (#peek struct sockaddr, sa_family) p
  case family :: CSaFamily of
#if defined(DOMAIN_SOCKET_SUPPORT)
    (#const AF_UNIX) -> do
        str <- peekCString ((#ptr struct sockaddr_un, sun_path) p)
        return (SockAddrUnix str)
#endif
    (#const AF_INET) -> do
        addr <- (#peek struct sockaddr_in, sin_addr) p
        port <- (#peek struct sockaddr_in, sin_port) p
        return (SockAddrInet (PortNum port) addr)
#if defined(IPV6_SOCKET_SUPPORT)
    (#const AF_INET6) -> do
        port <- (#peek struct sockaddr_in6, sin6_port) p
        flow <- (#peek struct sockaddr_in6, sin6_flowinfo) p
        addr <- (#peek struct sockaddr_in6, sin6_addr) p
        scope <- (#peek struct sockaddr_in6, sin6_scope_id) p
        return (SockAddrInet6 (PortNum port) flow addr scope)
#endif

------------------------------------------------------------------------
-- Protocol Families.

-- | This data type might have different constructors depending on
-- what is supported by the operating system.
data Family
    = AF_UNSPEC           -- unspecified
#ifdef AF_UNIX
    | AF_UNIX             -- local to host (pipes, portals
#endif
#ifdef AF_INET
    | AF_INET             -- internetwork: UDP, TCP, etc
#endif
#ifdef AF_INET6
    | AF_INET6            -- Internet Protocol version 6
#endif
#ifdef AF_IMPLINK
    | AF_IMPLINK          -- arpanet imp addresses
#endif
#ifdef AF_PUP
    | AF_PUP              -- pup protocols: e.g. BSP
#endif
#ifdef AF_CHAOS
    | AF_CHAOS            -- mit CHAOS protocols
#endif
#ifdef AF_NS
    | AF_NS               -- XEROX NS protocols
#endif
#ifdef AF_NBS
    | AF_NBS              -- nbs protocols
#endif
#ifdef AF_ECMA
    | AF_ECMA             -- european computer manufacturers
#endif
#ifdef AF_DATAKIT
    | AF_DATAKIT          -- datakit protocols
#endif
#ifdef AF_CCITT
    | AF_CCITT            -- CCITT protocols, X.25 etc
#endif
#ifdef AF_SNA
    | AF_SNA              -- IBM SNA
#endif
#ifdef AF_DECnet
    | AF_DECnet           -- DECnet
#endif
#ifdef AF_DLI
    | AF_DLI              -- Direct data link interface
#endif
#ifdef AF_LAT
    | AF_LAT              -- LAT
#endif
#ifdef AF_HYLINK
    | AF_HYLINK           -- NSC Hyperchannel
#endif
#ifdef AF_APPLETALK
    | AF_APPLETALK        -- Apple Talk
#endif
#ifdef AF_ROUTE
    | AF_ROUTE            -- Internal Routing Protocol
#endif
#ifdef AF_NETBIOS
    | AF_NETBIOS          -- NetBios-style addresses
#endif
#ifdef AF_NIT
    | AF_NIT              -- Network Interface Tap
#endif
#ifdef AF_802
    | AF_802              -- IEEE 802.2, also ISO 8802
#endif
#ifdef AF_ISO
    | AF_ISO              -- ISO protocols
#endif
#ifdef AF_OSI
    | AF_OSI              -- umbrella of all families used by OSI
#endif
#ifdef AF_NETMAN
    | AF_NETMAN           -- DNA Network Management
#endif
#ifdef AF_X25
    | AF_X25              -- CCITT X.25
#endif
#ifdef AF_AX25
    | AF_AX25
#endif
#ifdef AF_OSINET
    | AF_OSINET           -- AFI
#endif
#ifdef AF_GOSSIP
    | AF_GOSSIP           -- US Government OSI
#endif
#ifdef AF_IPX
    | AF_IPX              -- Novell Internet Protocol
#endif
#ifdef Pseudo_AF_XTP
    | Pseudo_AF_XTP       -- eXpress Transfer Protocol (no AF)
#endif
#ifdef AF_CTF
    | AF_CTF              -- Common Trace Facility
#endif
#ifdef AF_WAN
    | AF_WAN              -- Wide Area Network protocols
#endif
#ifdef AF_SDL
    | AF_SDL              -- SGI Data Link for DLPI
#endif
#ifdef AF_NETWARE
    | AF_NETWARE
#endif
#ifdef AF_NDD
    | AF_NDD
#endif
#ifdef AF_INTF
    | AF_INTF             -- Debugging use only
#endif
#ifdef AF_COIP
    | AF_COIP             -- connection-oriented IP, aka ST II
#endif
#ifdef AF_CNT
    | AF_CNT              -- Computer Network Technology
#endif
#ifdef Pseudo_AF_RTIP
    | Pseudo_AF_RTIP      -- Help Identify RTIP packets
#endif
#ifdef Pseudo_AF_PIP
    | Pseudo_AF_PIP       -- Help Identify PIP packets
#endif
#ifdef AF_SIP
    | AF_SIP              -- Simple Internet Protocol
#endif
#ifdef AF_ISDN
    | AF_ISDN             -- Integrated Services Digital Network
#endif
#ifdef Pseudo_AF_KEY
    | Pseudo_AF_KEY       -- Internal key-management function
#endif
#ifdef AF_NATM
    | AF_NATM             -- native ATM access
#endif
#ifdef AF_ARP
    | AF_ARP              -- (rev.) addr. res. prot. (RFC 826)
#endif
#ifdef Pseudo_AF_HDRCMPLT
    | Pseudo_AF_HDRCMPLT  -- Used by BPF to not rewrite hdrs in iface output
#endif
#ifdef AF_ENCAP
    | AF_ENCAP
#endif
#ifdef AF_LINK
    | AF_LINK             -- Link layer interface
#endif
#ifdef AF_RAW
    | AF_RAW              -- Link layer interface
#endif
#ifdef AF_RIF
    | AF_RIF              -- raw interface
#endif
#ifdef AF_NETROM
    | AF_NETROM           -- Amateur radio NetROM
#endif
#ifdef AF_BRIDGE
    | AF_BRIDGE           -- multiprotocol bridge
#endif
#ifdef AF_ATMPVC
    | AF_ATMPVC           -- ATM PVCs
#endif
#ifdef AF_ROSE
    | AF_ROSE             -- Amateur Radio X.25 PLP
#endif
#ifdef AF_NETBEUI
    | AF_NETBEUI          -- 802.2LLC
#endif
#ifdef AF_SECURITY
    | AF_SECURITY         -- Security callback pseudo AF
#endif
#ifdef AF_PACKET
    | AF_PACKET           -- Packet family
#endif
#ifdef AF_ASH
    | AF_ASH              -- Ash
#endif
#ifdef AF_ECONET
    | AF_ECONET           -- Acorn Econet
#endif
#ifdef AF_ATMSVC
    | AF_ATMSVC           -- ATM SVCs
#endif
#ifdef AF_IRDA
    | AF_IRDA             -- IRDA sockets
#endif
#ifdef AF_PPPOX
    | AF_PPPOX            -- PPPoX sockets
#endif
#ifdef AF_WANPIPE
    | AF_WANPIPE          -- Wanpipe API sockets
#endif
#ifdef AF_BLUETOOTH
    | AF_BLUETOOTH        -- bluetooth sockets
#endif
      deriving (Eq, Ord, Read, Show)

packFamily :: Family -> CInt
packFamily f = case f of
    AF_UNSPEC -> #const AF_UNSPEC
#ifdef AF_UNIX
    AF_UNIX -> #const AF_UNIX
#endif
#ifdef AF_INET
    AF_INET -> #const AF_INET
#endif
#ifdef AF_INET6
    AF_INET6 -> #const AF_INET6
#endif
#ifdef AF_IMPLINK
    AF_IMPLINK -> #const AF_IMPLINK
#endif
#ifdef AF_PUP
    AF_PUP -> #const AF_PUP
#endif
#ifdef AF_CHAOS
    AF_CHAOS -> #const AF_CHAOS
#endif
#ifdef AF_NS
    AF_NS -> #const AF_NS
#endif
#ifdef AF_NBS
    AF_NBS -> #const AF_NBS
#endif
#ifdef AF_ECMA
    AF_ECMA -> #const AF_ECMA
#endif
#ifdef AF_DATAKIT
    AF_DATAKIT -> #const AF_DATAKIT
#endif
#ifdef AF_CCITT
    AF_CCITT -> #const AF_CCITT
#endif
#ifdef AF_SNA
    AF_SNA -> #const AF_SNA
#endif
#ifdef AF_DECnet
    AF_DECnet -> #const AF_DECnet
#endif
#ifdef AF_DLI
    AF_DLI -> #const AF_DLI
#endif
#ifdef AF_LAT
    AF_LAT -> #const AF_LAT
#endif
#ifdef AF_HYLINK
    AF_HYLINK -> #const AF_HYLINK
#endif
#ifdef AF_APPLETALK
    AF_APPLETALK -> #const AF_APPLETALK
#endif
#ifdef AF_ROUTE
    AF_ROUTE -> #const AF_ROUTE
#endif
#ifdef AF_NETBIOS
    AF_NETBIOS -> #const AF_NETBIOS
#endif
#ifdef AF_NIT
    AF_NIT -> #const AF_NIT
#endif
#ifdef AF_802
    AF_802 -> #const AF_802
#endif
#ifdef AF_ISO
    AF_ISO -> #const AF_ISO
#endif
#ifdef AF_OSI
    AF_OSI -> #const AF_OSI
#endif
#ifdef AF_NETMAN
    AF_NETMAN -> #const AF_NETMAN
#endif
#ifdef AF_X25
    AF_X25 -> #const AF_X25
#endif
#ifdef AF_AX25
    AF_AX25 -> #const AF_AX25
#endif
#ifdef AF_OSINET
    AF_OSINET -> #const AF_OSINET
#endif
#ifdef AF_GOSSIP
    AF_GOSSIP -> #const AF_GOSSIP
#endif
#ifdef AF_IPX
    AF_IPX -> #const AF_IPX
#endif
#ifdef Pseudo_AF_XTP
    Pseudo_AF_XTP -> #const Pseudo_AF_XTP
#endif
#ifdef AF_CTF
    AF_CTF -> #const AF_CTF
#endif
#ifdef AF_WAN
    AF_WAN -> #const AF_WAN
#endif
#ifdef AF_SDL
    AF_SDL -> #const AF_SDL
#endif
#ifdef AF_NETWARE
    AF_NETWARE -> #const AF_NETWARE
#endif
#ifdef AF_NDD
    AF_NDD -> #const AF_NDD
#endif
#ifdef AF_INTF
    AF_INTF -> #const AF_INTF
#endif
#ifdef AF_COIP
    AF_COIP -> #const AF_COIP
#endif
#ifdef AF_CNT
    AF_CNT -> #const AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
    Pseudo_AF_RTIP -> #const Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
    Pseudo_AF_PIP -> #const Pseudo_AF_PIP
#endif
#ifdef AF_SIP
    AF_SIP -> #const AF_SIP
#endif
#ifdef AF_ISDN
    AF_ISDN -> #const AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
    Pseudo_AF_KEY -> #const Pseudo_AF_KEY
#endif
#ifdef AF_NATM
    AF_NATM -> #const AF_NATM
#endif
#ifdef AF_ARP
    AF_ARP -> #const AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
    Pseudo_AF_HDRCMPLT -> #const Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
    AF_ENCAP -> #const AF_ENCAP
#endif
#ifdef AF_LINK
    AF_LINK -> #const AF_LINK
#endif
#ifdef AF_RAW
    AF_RAW -> #const AF_RAW
#endif
#ifdef AF_RIF
    AF_RIF -> #const AF_RIF
#endif
#ifdef AF_NETROM
    AF_NETROM -> #const AF_NETROM
#endif
#ifdef AF_BRIDGE
    AF_BRIDGE -> #const AF_BRIDGE
#endif
#ifdef AF_ATMPVC
    AF_ATMPVC -> #const AF_ATMPVC
#endif
#ifdef AF_ROSE
    AF_ROSE -> #const AF_ROSE
#endif
#ifdef AF_NETBEUI
    AF_NETBEUI -> #const AF_NETBEUI
#endif
#ifdef AF_SECURITY
    AF_SECURITY -> #const AF_SECURITY
#endif
#ifdef AF_PACKET
    AF_PACKET -> #const AF_PACKET
#endif
#ifdef AF_ASH
    AF_ASH -> #const AF_ASH
#endif
#ifdef AF_ECONET
    AF_ECONET -> #const AF_ECONET
#endif
#ifdef AF_ATMSVC
    AF_ATMSVC -> #const AF_ATMSVC
#endif
#ifdef AF_IRDA
    AF_IRDA -> #const AF_IRDA
#endif
#ifdef AF_PPPOX
    AF_PPPOX -> #const AF_PPPOX
#endif
#ifdef AF_WANPIPE
    AF_WANPIPE -> #const AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
    AF_BLUETOOTH -> #const AF_BLUETOOTH
#endif

unpackFamily :: CInt -> Family
unpackFamily f = case f of
    (#const AF_UNSPEC) -> AF_UNSPEC
#ifdef AF_UNIX
    (#const AF_UNIX) -> AF_UNIX
#endif
#ifdef AF_INET
    (#const AF_INET) -> AF_INET
#endif
#ifdef AF_INET6
    (#const AF_INET6) -> AF_INET6
#endif
#ifdef AF_IMPLINK
    (#const AF_IMPLINK) -> AF_IMPLINK
#endif
#ifdef AF_PUP
    (#const AF_PUP) -> AF_PUP
#endif
#ifdef AF_CHAOS
    (#const AF_CHAOS) -> AF_CHAOS
#endif
#ifdef AF_NS
    (#const AF_NS) -> AF_NS
#endif
#ifdef AF_NBS
    (#const AF_NBS) -> AF_NBS
#endif
#ifdef AF_ECMA
    (#const AF_ECMA) -> AF_ECMA
#endif
#ifdef AF_DATAKIT
    (#const AF_DATAKIT) -> AF_DATAKIT
#endif
#ifdef AF_CCITT
    (#const AF_CCITT) -> AF_CCITT
#endif
#ifdef AF_SNA
    (#const AF_SNA) -> AF_SNA
#endif
#ifdef AF_DECnet
    (#const AF_DECnet) -> AF_DECnet
#endif
#ifdef AF_DLI
    (#const AF_DLI) -> AF_DLI
#endif
#ifdef AF_LAT
    (#const AF_LAT) -> AF_LAT
#endif
#ifdef AF_HYLINK
    (#const AF_HYLINK) -> AF_HYLINK
#endif
#ifdef AF_APPLETALK
    (#const AF_APPLETALK) -> AF_APPLETALK
#endif
#ifdef AF_ROUTE
    (#const AF_ROUTE) -> AF_ROUTE
#endif
#ifdef AF_NETBIOS
    (#const AF_NETBIOS) -> AF_NETBIOS
#endif
#ifdef AF_NIT
    (#const AF_NIT) -> AF_NIT
#endif
#ifdef AF_802
    (#const AF_802) -> AF_802
#endif
#ifdef AF_ISO
    (#const AF_ISO) -> AF_ISO
#endif
#ifdef AF_OSI
# if (!defined(AF_ISO)) || (defined(AF_ISO) && (AF_ISO != AF_OSI))
    (#const AF_OSI) -> AF_OSI
# endif
#endif
#ifdef AF_NETMAN
    (#const AF_NETMAN) -> AF_NETMAN
#endif
#ifdef AF_X25
    (#const AF_X25) -> AF_X25
#endif
#ifdef AF_AX25
    (#const AF_AX25) -> AF_AX25
#endif
#ifdef AF_OSINET
    (#const AF_OSINET) -> AF_OSINET
#endif
#ifdef AF_GOSSIP
    (#const AF_GOSSIP) -> AF_GOSSIP
#endif
#if defined(AF_IPX) && (!defined(AF_NS) || AF_NS != AF_IPX)
    (#const AF_IPX) -> AF_IPX
#endif
#ifdef Pseudo_AF_XTP
    (#const Pseudo_AF_XTP) -> Pseudo_AF_XTP
#endif
#ifdef AF_CTF
    (#const AF_CTF) -> AF_CTF
#endif
#ifdef AF_WAN
    (#const AF_WAN) -> AF_WAN
#endif
#ifdef AF_SDL
    (#const AF_SDL) -> AF_SDL
#endif
#ifdef AF_NETWARE
    (#const AF_NETWARE) -> AF_NETWARE
#endif
#ifdef AF_NDD
    (#const AF_NDD) -> AF_NDD
#endif
#ifdef AF_INTF
    (#const AF_INTF) -> AF_INTF
#endif
#ifdef AF_COIP
    (#const AF_COIP) -> AF_COIP
#endif
#ifdef AF_CNT
    (#const AF_CNT) -> AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
    (#const Pseudo_AF_RTIP) -> Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
    (#const Pseudo_AF_PIP) -> Pseudo_AF_PIP
#endif
#ifdef AF_SIP
    (#const AF_SIP) -> AF_SIP
#endif
#ifdef AF_ISDN
    (#const AF_ISDN) -> AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
    (#const Pseudo_AF_KEY) -> Pseudo_AF_KEY
#endif
#ifdef AF_NATM
    (#const AF_NATM) -> AF_NATM
#endif
#ifdef AF_ARP
    (#const AF_ARP) -> AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
    (#const Pseudo_AF_HDRCMPLT) -> Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
    (#const AF_ENCAP) -> AF_ENCAP
#endif
#ifdef AF_LINK
    (#const AF_LINK) -> AF_LINK
#endif
#ifdef AF_RAW
    (#const AF_RAW) -> AF_RAW
#endif
#ifdef AF_RIF
    (#const AF_RIF) -> AF_RIF
#endif
#ifdef AF_NETROM
    (#const AF_NETROM) -> AF_NETROM
#endif
#ifdef AF_BRIDGE
    (#const AF_BRIDGE) -> AF_BRIDGE
#endif
#ifdef AF_ATMPVC
    (#const AF_ATMPVC) -> AF_ATMPVC
#endif
#ifdef AF_ROSE
    (#const AF_ROSE) -> AF_ROSE
#endif
#ifdef AF_NETBEUI
    (#const AF_NETBEUI) -> AF_NETBEUI
#endif
#ifdef AF_SECURITY
    (#const AF_SECURITY) -> AF_SECURITY
#endif
#ifdef AF_PACKET
    (#const AF_PACKET) -> AF_PACKET
#endif
#ifdef AF_ASH
    (#const AF_ASH) -> AF_ASH
#endif
#ifdef AF_ECONET
    (#const AF_ECONET) -> AF_ECONET
#endif
#ifdef AF_ATMSVC
    (#const AF_ATMSVC) -> AF_ATMSVC
#endif
#ifdef AF_IRDA
    (#const AF_IRDA) -> AF_IRDA
#endif
#ifdef AF_PPPOX
    (#const AF_PPPOX) -> AF_PPPOX
#endif
#ifdef AF_WANPIPE
    (#const AF_WANPIPE) -> AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
    (#const AF_BLUETOOTH) -> AF_BLUETOOTH
#endif
    unknown -> error ("Network.Socket.unpackFamily: unknown address " ++
               "family " ++ show unknown)

-- ---------------------------------------------------------------------
-- Guards for socket operations that may fail

-- | Throw an 'IOError' corresponding to the current socket error.
throwSocketError :: String  -- ^ textual description of the error location
                 -> IO a

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()

{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a
{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

-- | As 'throwSocketErrorIfMinus1Retry', but discards the result.
throwSocketErrorIfMinus1Retry_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
throwSocketErrorIfMinus1Retry_ name act =
    throwSocketErrorIfMinus1Retry name act >> return ()
{-# SPECIALIZE throwSocketErrorIfMinus1Retry_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.  Checks for operations that would block and
-- executes an alternative action before retrying in that case.
throwSocketErrorIfMinus1RetryMayBlock
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO b    -- ^ action to execute before retrying if an
               --   immediate retry would block
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a

{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}

#if defined(__GLASGOW_HASKELL__) && (!defined(HAVE_WINSOCK2_H) || defined(cygwin32_HOST_OS))

throwSocketErrorIfMinus1RetryMayBlock name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block

throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry

throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_

throwSocketError = throwErrno

#else

throwSocketErrorIfMinus1RetryMayBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

throwSocketErrorIfMinus1_ name act = do
  throwSocketErrorIfMinus1Retry name act
  return ()

# if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
throwSocketErrorIfMinus1Retry name act = do
  r <- act
  if (r == -1)
   then do
    rc   <- c_getLastError
    case rc of
      10093 -> do -- WSANOTINITIALISED
        withSocketsDo (return ())
        r <- act
        if (r == -1)
           then throwSocketError name
           else return r
      _ -> throwSocketError name
   else return r

throwSocketError name = do
    rc <- c_getLastError
    pstr <- c_getWSError rc
    str  <- peekCString pstr
#  if __GLASGOW_HASKELL__
    ioError (ioeSetErrorString (mkIOError OtherError name Nothing Nothing) str)
#  else
    ioError (userError (name ++ ": socket error - " ++ str))
#  endif

foreign import CALLCONV unsafe "WSAGetLastError"
  c_getLastError :: IO CInt

foreign import ccall unsafe "getWSErrorDescr"
  c_getWSError :: CInt -> IO (Ptr CChar)


# else
throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry
throwSocketError = throwErrno
# endif
#endif /* __GLASGOW_HASKELL */

-- ---------------------------------------------------------------------------
-- WinSock support

{-| On Windows operating systems, the networking subsystem has to be
initialised using 'withSocketsDo' before any networking operations can
be used.  eg.

> main = withSocketsDo $ do {...}

Although this is only strictly necessary on Windows platforms, it is
harmless on other platforms, so for portability it is good practice to
use it all the time.
-}
withSocketsDo :: IO a -> IO a
#if !defined(WITH_WINSOCK)
withSocketsDo x = x
#else
withSocketsDo act = do
    x <- initWinSock
    if x /= 0
       then ioError (userError "Failed to initialise WinSock")
       else act `finally` shutdownWinSock

foreign import ccall unsafe "initWinSock" initWinSock :: IO Int
foreign import ccall unsafe "shutdownWinSock" shutdownWinSock :: IO ()

#endif

------------------------------------------------------------------------
-- Helper functions

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
