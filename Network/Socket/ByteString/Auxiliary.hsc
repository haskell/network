{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.ByteString.Auxiliary where

#include "HsNet.h"

#include <sys/types.h>
#include <sys/socket.h>

import Data.ByteString.Internal
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafeDupablePerformIO)

import Network.Socket.ByteString.Cmsg
import Network.Socket.Imports
import Network.Socket.Types

----------------------------------------------------------------

-- | Identifier of auxiliary data. A pair of level and type.
type AuxiliaryID = (CInt, CInt)

-- | The identifier for 'IPv4TTL'.
auxiliaryIPv4TTL :: AuxiliaryID
#if defined(darwin_HOST_OS)
auxiliaryIPv4TTL = ((#const IPPROTO_IP), (#const IP_RECVTTL))
#else
auxiliaryIPv4TTL = ((#const IPPROTO_IP), (#const IP_TTL))
#endif

-- | The identifier for 'IPv6HopLimit'.
auxiliaryIPv6HopLimit :: AuxiliaryID
auxiliaryIPv6HopLimit = ((#const IPPROTO_IPV6), (#const IPV6_HOPLIMIT))

-- | The identifier for 'IPv4TOS'.
auxiliaryIPv4TOS :: AuxiliaryID
#if defined(darwin_HOST_OS)
auxiliaryIPv4TOS = ((#const IPPROTO_IP), (#const IP_RECVTOS))
#else
auxiliaryIPv4TOS = ((#const IPPROTO_IP), (#const IP_TOS))
#endif

-- | The identifier for 'IPv6TClass'.
auxiliaryIPv6TClass :: AuxiliaryID
auxiliaryIPv6TClass = ((#const IPPROTO_IPV6), (#const IPV6_TCLASS))

-- | The identifier for 'IPv4PktInfo'.
auxiliaryIPv4PktInfo :: AuxiliaryID
auxiliaryIPv4PktInfo = ((#const IPPROTO_IP), (#const IP_PKTINFO))

-- | The identifier for 'IPv6PktInfo'.
auxiliaryIPv6PktInfo :: AuxiliaryID
auxiliaryIPv6PktInfo = ((#const IPPROTO_IPV6), (#const IPV6_PKTINFO))

----------------------------------------------------------------

-- | Looking up auxiliary data. The following shows an example usage:
--
-- > (lookupAuxiliary auxiliaryIPv4TOS cmsgs >>= auxiliaryDecode) :: Maybe IPv4TOS
lookupAuxiliary :: AuxiliaryID -> [Cmsg] -> Maybe Cmsg
lookupAuxiliary _   [] = Nothing
lookupAuxiliary aid (cmsg@(Cmsg cid _):cmsgs)
  | aid == cid = Just cmsg
  | otherwise  = lookupAuxiliary aid cmsgs

----------------------------------------------------------------

-- | A class to encode and decode auxiliary data.
class Auxiliary a where
    auxiliaryEncode :: a -> Cmsg
    auxiliaryDecode :: Cmsg -> Maybe a

----------------------------------------------------------------

packCInt :: CInt -> ByteString
packCInt n = unsafeDupablePerformIO $ create siz $ \p0 -> do
    let p = castPtr p0 :: Ptr CInt
    poke p n
  where
    siz = (#size int)

unpackCInt :: ByteString -> Maybe CInt
unpackCInt (PS fptr off len)
  | len < siz = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off) :: Ptr CInt
        Just <$> peek p
  where
    siz = (#size int)

packCChar :: CChar -> ByteString
packCChar n = unsafeDupablePerformIO $ create siz $ \p0 -> do
    let p = castPtr p0 :: Ptr CChar
    poke p n
  where
    siz = (#size char)

unpackCChar :: ByteString -> Maybe CChar
unpackCChar (PS fptr off len)
  | len < siz = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off) :: Ptr CChar
        Just <$> peek p
  where
    siz = (#size char)

----------------------------------------------------------------

-- | Time to live of IPv4.
newtype IPv4TTL = IPv4TTL Int deriving (Eq, Show)

instance Auxiliary IPv4TTL where
#if defined(darwin_HOST_OS)
    auxiliaryEncode (IPv4TTL ttl) = Cmsg auxiliaryIPv4TTL $ packCChar $ fromIntegral ttl
#else
    auxiliaryEncode (IPv4TTL ttl) = Cmsg auxiliaryIPv4TTL $ packCInt $ fromIntegral ttl
#endif
#if defined(darwin_HOST_OS)
    auxiliaryDecode (Cmsg _ bs)   = IPv4TTL . fromIntegral <$> unpackCChar bs
#else
    auxiliaryDecode (Cmsg _ bs)   = IPv4TTL . fromIntegral <$> unpackCInt bs
#endif

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit Int deriving (Eq, Show)

instance Auxiliary IPv6HopLimit where
    auxiliaryEncode (IPv6HopLimit ttl) = Cmsg auxiliaryIPv6HopLimit $ packCInt $ fromIntegral ttl
    auxiliaryDecode (Cmsg _ bs)        = IPv6HopLimit . fromIntegral <$> unpackCInt bs

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS Int deriving (Eq, Show)

instance Auxiliary IPv4TOS where
    auxiliaryEncode (IPv4TOS ttl) = Cmsg auxiliaryIPv4TOS $ packCChar $ fromIntegral ttl
    auxiliaryDecode (Cmsg _ bs)   = IPv4TOS . fromIntegral <$> unpackCChar bs

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass Int deriving (Eq, Show)

instance Auxiliary IPv6TClass where
    auxiliaryEncode (IPv6TClass ttl) = Cmsg auxiliaryIPv6TClass $ packCInt $ fromIntegral ttl
    auxiliaryDecode (Cmsg _ bs)      = IPv6TClass . fromIntegral <$> unpackCInt bs

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv4PktInfo = IPv4PktInfo Int HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple ha)

instance Auxiliary IPv4PktInfo where
    auxiliaryEncode pktinfo = Cmsg auxiliaryIPv4PktInfo $ packIPv4PktInfo pktinfo
    auxiliaryDecode (Cmsg _ bs) = unpackIPv4PktInfo bs

{-# NOINLINE packIPv4PktInfo #-}
packIPv4PktInfo :: IPv4PktInfo -> ByteString
packIPv4PktInfo (IPv4PktInfo n ha) = unsafeDupablePerformIO $
    create siz $ \p -> do
        (#poke struct in_pktinfo, ipi_ifindex)  p (fromIntegral n :: CInt)
        (#poke struct in_pktinfo, ipi_spec_dst) p (0 :: CInt)
        (#poke struct in_pktinfo, ipi_addr)     p ha
  where
    siz = (#size struct in_pktinfo)

{-# NOINLINE unpackIPv4PktInfo #-}
unpackIPv4PktInfo :: ByteString -> Maybe IPv4PktInfo
unpackIPv4PktInfo (PS fptr off len)
  | len < siz = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = p0 `plusPtr` off
        n  <- (#peek struct in_pktinfo, ipi_ifindex) p
        ha <- (#peek struct in_pktinfo, ipi_addr)    p
        return $ Just $ IPv4PktInfo n ha
  where
    siz = (#size struct in_pktinfo)

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv6PktInfo = IPv6PktInfo Int HostAddress6 deriving (Eq)

instance Show IPv6PktInfo where
    show (IPv6PktInfo n ha6) = "IPv6PktInfo " ++ show n ++ " " ++ show (hostAddress6ToTuple ha6)

instance Auxiliary IPv6PktInfo where
    auxiliaryEncode pktinfo = Cmsg auxiliaryIPv6PktInfo $ packIPv6PktInfo pktinfo
    auxiliaryDecode (Cmsg _ bs) = unpackIPv6PktInfo bs

{-# NOINLINE packIPv6PktInfo #-}
packIPv6PktInfo :: IPv6PktInfo -> ByteString
packIPv6PktInfo (IPv6PktInfo n ha6) = unsafeDupablePerformIO $
    create siz $ \p -> do
        (#poke struct in6_pktinfo, ipi6_ifindex) p (fromIntegral n :: CInt)
        (#poke struct in6_pktinfo, ipi6_addr)    p (In6Addr ha6)
  where
    siz = (#size struct in6_pktinfo)

{-# NOINLINE unpackIPv6PktInfo #-}
unpackIPv6PktInfo :: ByteString -> Maybe IPv6PktInfo
unpackIPv6PktInfo (PS fptr off len)
  | len < siz = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = p0 `plusPtr` off
        In6Addr ha6 <- (#peek struct in6_pktinfo, ipi6_addr)    p
        n :: CInt   <- (#peek struct in6_pktinfo, ipi6_ifindex) p
        return $ Just $ IPv6PktInfo (fromIntegral n) ha6
  where
    siz = (#size struct in6_pktinfo)
