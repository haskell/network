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

#if defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS)
#define WITH_WINSOCK  1
#endif

#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
#define DOMAIN_SOCKET_SUPPORT 1
#endif

module Network.Socket.Internal
    ( HostAddress,
#if defined(IPV6_SOCKET_SUPPORT)
      HostAddress6,
      FlowInfo,
      ScopeID,
#endif
      PortNumber(..),
      SockAddr(..),

      -- * Marshalling
      peekSockAddr,
      pokeSockAddr,
      sizeOfSockAddr,
      withSockAddr,
    ) where

import Data.Bits ( (.|.), shiftL, shiftR )
import Data.Word ( Word8, Word16, Word32 )
import Foreign.C.String ( castCharToCChar, peekCString )
import Foreign.C.Types ( CInt, CSize )
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Marshal.Array ( pokeArray, pokeArray0 )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(..) )

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
-- SockAddr

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

data SockAddr		-- C Names
  = SockAddrInet
	PortNumber	-- sin_port  (network byte order)
	HostAddress	-- sin_addr  (ditto)
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

-- size of struct sockaddr by SockAddr
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

withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
 let sz = sizeOfSockAddr addr
 allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

-- We can't write an instance of 'Storable' for 'SockAddr' because
-- @sockaddr@ is a sum type of variable size but
-- 'Foreign.Storable.sizeOf' is required to be constant.

-- Note that on Darwin, the sockaddr structure must be zeroed before
-- use.
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

-- helper function used to zero a structure
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()
