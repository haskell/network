{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.Internal
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
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

#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
#define WITH_WINSOCK  1
#endif

#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
#define DOMAIN_SOCKET_SUPPORT 1
#endif

#if !defined(CALLCONV)
#ifdef WITH_WINSOCK
#define CALLCONV stdcall
#else
#define CALLCONV ccall
#endif
#endif

module Network.Socket.Internal
    (
      -- * Socket addresses
      HostAddress,
#if defined(IPV6_SOCKET_SUPPORT)
      HostAddress6,
      FlowInfo,
      ScopeID,
#endif
      PortNumber(..),
      SockAddr(..),

      peekSockAddr,
      pokeSockAddr,
      sizeOfSockAddr,
      sizeOfSockAddrByFamily,
      withSockAddr,
      withNewSockAddr,

      -- * Protocol families
      Family(..),

      -- * Socket error functions
#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
      c_getLastError,
#endif
      throwSocketError,

      -- * Guards for socket operations that may fail
      throwSocketErrorIfMinus1_,
      throwSocketErrorIfMinus1Retry,
      throwSocketErrorIfMinus1RetryMayBlock,

      -- * Initialization
      withSocketsDo,
    ) where

import Data.Bits ( (.|.), shiftL, shiftR )
import Data.Word ( Word8, Word16, Word32 )
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1Retry,
                        throwErrnoIfMinus1RetryMayBlock, throwErrnoIfMinus1_)
import Foreign.C.String ( castCharToCChar, peekCString )
import Foreign.C.Types ( CInt, CSize )
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

type HostAddress = Word32

#if defined(IPV6_SOCKET_SUPPORT)
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
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i a = do
    let i' = i * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        a `sr` i = fromIntegral (a `shiftR` i) :: Word8
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

newtype PortNumber = PortNum Word16 deriving ( Eq, Ord )

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
type FlowInfo = Word32
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
  deriving (Eq)

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

-- ---------------------------------------------------------------------
-- Guards for socket operations that may fail

-- | Throw an 'IOError' corresponding to the current socket error.
throwSocketError :: String  -- ^ textual description of the error location
                 -> IO a

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: Num a => String  -- ^ textual description of the location
    -> IO a             -- ^ the 'IO' operation to be executed
    -> IO ()

{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: Num a => String  -- ^ textual description of the location
    -> IO a             -- ^ the 'IO' operation to be executed
    -> IO a

{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.  Checks for operations that would block and
-- executes an alternative action before retrying in that case.
throwSocketErrorIfMinus1RetryMayBlock
    :: Num a => String  -- ^ textual description of the location
    -> IO b             -- ^ action to execute before retrying if an
                        --   immediate retry would block
    -> IO a             -- ^ the 'IO' operation to be executed
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
