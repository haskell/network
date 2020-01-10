{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.Posix.Ancillary where

#include "HsNet.h"

#include <sys/types.h>
#include <sys/socket.h>

import Data.ByteString.Internal
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Types (Fd(..))

import Network.Socket.Imports
import Network.Socket.Posix.Cmsg
import Network.Socket.Types

----------------------------------------------------------------

-- | Identifier of ancillary data. A pair of level and type.
type AncillaryId = (CInt, CInt)

-- | The identifier for 'IPv4TTL'.
ancillaryIPv4TTL :: AncillaryId
#if defined(darwin_HOST_OS)
ancillaryIPv4TTL = ((#const IPPROTO_IP), (#const IP_RECVTTL))
#else
ancillaryIPv4TTL = ((#const IPPROTO_IP), (#const IP_TTL))
#endif

-- | The identifier for 'IPv6HopLimit'.
ancillaryIPv6HopLimit :: AncillaryId
ancillaryIPv6HopLimit = ((#const IPPROTO_IPV6), (#const IPV6_HOPLIMIT))

-- | The identifier for 'IPv4TOS'.
ancillaryIPv4TOS :: AncillaryId
#if defined(darwin_HOST_OS)
ancillaryIPv4TOS = ((#const IPPROTO_IP), (#const IP_RECVTOS))
#else
ancillaryIPv4TOS = ((#const IPPROTO_IP), (#const IP_TOS))
#endif

-- | The identifier for 'IPv6TClass'.
ancillaryIPv6TClass :: AncillaryId
ancillaryIPv6TClass = ((#const IPPROTO_IPV6), (#const IPV6_TCLASS))

-- | The identifier for 'IPv4PktInfo'.
ancillaryIPv4PktInfo :: AncillaryId
ancillaryIPv4PktInfo = ((#const IPPROTO_IP), (#const IP_PKTINFO))

-- | The identifier for 'IPv6PktInfo'.
ancillaryIPv6PktInfo :: AncillaryId
ancillaryIPv6PktInfo = ((#const IPPROTO_IPV6), (#const IPV6_PKTINFO))

-- | The identifier for 'Fd'.
ancillaryFd :: AncillaryId
ancillaryFd = ((#const SOL_SOCKET), (#const SCM_RIGHTS))

----------------------------------------------------------------

-- | Looking up ancillary data. The following shows an example usage:
--
-- > (lookupAncillary ancillaryIPv4TOS cmsgs >>= ancillaryDecode) :: Maybe IPv4TOS
lookupAncillary :: AncillaryId -> [Cmsg] -> Maybe Cmsg
lookupAncillary _   [] = Nothing
lookupAncillary aid (cmsg@(Cmsg cid _):cmsgs)
  | aid == cid = Just cmsg
  | otherwise  = lookupAncillary aid cmsgs

----------------------------------------------------------------

-- | A class to encode and decode ancillary data.
class Storable a => Ancillary a where
    ancillaryId :: a -> AncillaryId

ancillaryEncode :: Ancillary a => a -> Cmsg
ancillaryEncode x = unsafeDupablePerformIO $ do
    bs <- create siz $ \p0 -> do
        let p = castPtr p0
        poke p x
    return $ Cmsg (ancillaryId x) bs
  where
    siz = sizeOf x

ancillaryDecode :: forall a . Storable a => Cmsg -> Maybe a
ancillaryDecode (Cmsg _ (PS fptr off len))
  | len < siz = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off)
        Just <$> peek p
  where
    siz = sizeOf (undefined :: a)

----------------------------------------------------------------

-- | Time to live of IPv4.
newtype IPv4TTL = IPv4TTL CChar deriving (Eq, Show, Storable)

instance Ancillary IPv4TTL where
    ancillaryId _ = ancillaryIPv4TTL

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit CInt deriving (Eq, Show, Storable)

instance Ancillary IPv6HopLimit where
    ancillaryId _ = ancillaryIPv6HopLimit

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS CChar deriving (Eq, Show, Storable)

instance Ancillary IPv4TOS where
    ancillaryId _ = ancillaryIPv4TOS

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass CInt deriving (Eq, Show, Storable)

instance Ancillary IPv6TClass where
    ancillaryId _ = ancillaryIPv6TClass

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv4PktInfo = IPv4PktInfo CInt HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple ha)

instance Ancillary IPv4PktInfo where
    ancillaryId _ = ancillaryIPv4PktInfo

instance Storable IPv4PktInfo where
    sizeOf _ = (#size struct in_pktinfo)
    alignment = undefined
    poke p (IPv4PktInfo n ha) = do
        (#poke struct in_pktinfo, ipi_ifindex)  p (fromIntegral n :: CInt)
        (#poke struct in_pktinfo, ipi_spec_dst) p (0 :: CInt)
        (#poke struct in_pktinfo, ipi_addr)     p ha
    peek p = do
        n  <- (#peek struct in_pktinfo, ipi_ifindex) p
        ha <- (#peek struct in_pktinfo, ipi_addr)    p
        return $ IPv4PktInfo n ha

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv6PktInfo = IPv6PktInfo Int HostAddress6 deriving (Eq)

instance Show IPv6PktInfo where
    show (IPv6PktInfo n ha6) = "IPv6PktInfo " ++ show n ++ " " ++ show (hostAddress6ToTuple ha6)

instance Ancillary IPv6PktInfo where
    ancillaryId _ = ancillaryIPv6PktInfo

instance Storable IPv6PktInfo where
    sizeOf _ = (#size struct in6_pktinfo)
    alignment = undefined
    poke p (IPv6PktInfo n ha6) = do
        (#poke struct in6_pktinfo, ipi6_ifindex) p (fromIntegral n :: CInt)
        (#poke struct in6_pktinfo, ipi6_addr)    p (In6Addr ha6)
    peek p = do
        In6Addr ha6 <- (#peek struct in6_pktinfo, ipi6_addr)    p
        n :: CInt   <- (#peek struct in6_pktinfo, ipi6_ifindex) p
        return $ IPv6PktInfo (fromIntegral n) ha6

----------------------------------------------------------------

instance Ancillary Fd where
    ancillaryId _ = ancillaryFd
