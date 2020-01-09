{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.ByteString.Ancillary where

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

-- | Identifier of ancillary data. A pair of level and type.
type AncillaryID = (CInt, CInt)

-- | The identifier for 'IPv4TTL'.
ancillaryIPv4TTL :: AncillaryID
#if defined(darwin_HOST_OS)
ancillaryIPv4TTL = ((#const IPPROTO_IP), (#const IP_RECVTTL))
#else
ancillaryIPv4TTL = ((#const IPPROTO_IP), (#const IP_TTL))
#endif

-- | The identifier for 'IPv6HopLimit'.
ancillaryIPv6HopLimit :: AncillaryID
ancillaryIPv6HopLimit = ((#const IPPROTO_IPV6), (#const IPV6_HOPLIMIT))

-- | The identifier for 'IPv4TOS'.
ancillaryIPv4TOS :: AncillaryID
#if defined(darwin_HOST_OS)
ancillaryIPv4TOS = ((#const IPPROTO_IP), (#const IP_RECVTOS))
#else
ancillaryIPv4TOS = ((#const IPPROTO_IP), (#const IP_TOS))
#endif

-- | The identifier for 'IPv6TClass'.
ancillaryIPv6TClass :: AncillaryID
ancillaryIPv6TClass = ((#const IPPROTO_IPV6), (#const IPV6_TCLASS))

-- | The identifier for 'IPv4PktInfo'.
ancillaryIPv4PktInfo :: AncillaryID
ancillaryIPv4PktInfo = ((#const IPPROTO_IP), (#const IP_PKTINFO))

-- | The identifier for 'IPv6PktInfo'.
ancillaryIPv6PktInfo :: AncillaryID
ancillaryIPv6PktInfo = ((#const IPPROTO_IPV6), (#const IPV6_PKTINFO))

----------------------------------------------------------------

-- | Looking up ancillary data. The following shows an example usage:
--
-- > (lookupAncillary ancillaryIPv4TOS cmsgs >>= ancillaryDecode) :: Maybe IPv4TOS
lookupAncillary :: AncillaryID -> [Cmsg] -> Maybe Cmsg
lookupAncillary _   [] = Nothing
lookupAncillary aid (cmsg@(Cmsg cid _):cmsgs)
  | aid == cid = Just cmsg
  | otherwise  = lookupAncillary aid cmsgs

----------------------------------------------------------------

-- | A class to encode and decode ancillary data.
class Ancillary a where
    ancillaryEncode :: a -> Cmsg
    ancillaryDecode :: Cmsg -> Maybe a

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

instance Ancillary IPv4TTL where
#if defined(darwin_HOST_OS)
    ancillaryEncode (IPv4TTL ttl) = Cmsg ancillaryIPv4TTL $ packCChar $ fromIntegral ttl
#else
    ancillaryEncode (IPv4TTL ttl) = Cmsg ancillaryIPv4TTL $ packCInt $ fromIntegral ttl
#endif
#if defined(darwin_HOST_OS)
    ancillaryDecode (Cmsg _ bs)   = IPv4TTL . fromIntegral <$> unpackCChar bs
#else
    ancillaryDecode (Cmsg _ bs)   = IPv4TTL . fromIntegral <$> unpackCInt bs
#endif

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit Int deriving (Eq, Show)

instance Ancillary IPv6HopLimit where
    ancillaryEncode (IPv6HopLimit ttl) = Cmsg ancillaryIPv6HopLimit $ packCInt $ fromIntegral ttl
    ancillaryDecode (Cmsg _ bs)        = IPv6HopLimit . fromIntegral <$> unpackCInt bs

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS Int deriving (Eq, Show)

instance Ancillary IPv4TOS where
    ancillaryEncode (IPv4TOS ttl) = Cmsg ancillaryIPv4TOS $ packCChar $ fromIntegral ttl
    ancillaryDecode (Cmsg _ bs)   = IPv4TOS . fromIntegral <$> unpackCChar bs

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass Int deriving (Eq, Show)

instance Ancillary IPv6TClass where
    ancillaryEncode (IPv6TClass ttl) = Cmsg ancillaryIPv6TClass $ packCInt $ fromIntegral ttl
    ancillaryDecode (Cmsg _ bs)      = IPv6TClass . fromIntegral <$> unpackCInt bs

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv4PktInfo = IPv4PktInfo Int HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple ha)

instance Ancillary IPv4PktInfo where
    ancillaryEncode pktinfo = Cmsg ancillaryIPv4PktInfo $ packIPv4PktInfo pktinfo
    ancillaryDecode (Cmsg _ bs) = unpackIPv4PktInfo bs

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

instance Ancillary IPv6PktInfo where
    ancillaryEncode pktinfo = Cmsg ancillaryIPv6PktInfo $ packIPv6PktInfo pktinfo
    ancillaryDecode (Cmsg _ bs) = unpackIPv6PktInfo bs

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
