{-# LANGUAGE CPP, ForeignFunctionInterface #-}

#include <netinet/in.h>

-- | Support module for 'struct sockaddr'.
module Network.Socket.ByteString.SockAddr
  ( pokeSockAddr
  , sizeOfSockAddr
  ) where

import Data.Word (Word8, Word16)
import Foreign.C.String (castCharToCChar)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Array (pokeArray, pokeArray0)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import Network.Socket (PortNumber(PortNum), SockAddr(SockAddrUnix, SockAddrInet, SockAddrInet6))

pokeSockAddr :: Ptr SockAddr -> SockAddr -> IO ()
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

sizeOfSockAddr :: Int
sizeOfSockAddr = (#const sizeof(struct sockaddr))

-- ----------------------------------------

type CSaFamily = Word16

zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory ptr n = c_memset ptr 0 (fromIntegral n)

foreign import ccall unsafe "memset"
  c_memset :: Ptr a -> CInt -> CSize -> IO ()
