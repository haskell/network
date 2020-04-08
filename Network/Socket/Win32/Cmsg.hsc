{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socket.Win32.Cmsg where

#include "HsNet.h"

import Data.ByteString.Internal
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafeDupablePerformIO)

import Network.Socket.Imports
import Network.Socket.Types

type DWORD = Word32
type ULONG = Word32

-- | Control message (ancillary data) including a pair of level and type.
data Cmsg = Cmsg {
    cmsgId   :: !CmsgId
  , cmsgData :: !ByteString
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | Identifier of control message (ancillary data).
data CmsgId = CmsgId {
    cmsgLevel :: !CInt
  , cmsglType :: !CInt
  } deriving (Eq, Show)

-- | The identifier for 'IPv4TTL'.
pattern CmsgIdIPv4TTL :: CmsgId
pattern CmsgIdIPv4TTL = CmsgId (#const IPPROTO_IP) (#const IP_TTL)

-- | The identifier for 'IPv6HopLimit'.
pattern CmsgIdIPv6HopLimit :: CmsgId
pattern CmsgIdIPv6HopLimit = CmsgId (#const IPPROTO_IPV6) (#const IPV6_HOPLIMIT)

-- | The identifier for 'IPv4TOS'.
pattern CmsgIdIPv4TOS :: CmsgId
pattern CmsgIdIPv4TOS = CmsgId (#const IPPROTO_IP) (#const IP_TOS)

-- | The identifier for 'IPv6TClass'.
pattern CmsgIdIPv6TClass :: CmsgId
pattern CmsgIdIPv6TClass = CmsgId (#const IPPROTO_IPV6) (#const IPV6_TCLASS)

-- | The identifier for 'IPv4PktInfo'.
pattern CmsgIdIPv4PktInfo :: CmsgId
pattern CmsgIdIPv4PktInfo = CmsgId (#const IPPROTO_IP) (#const IP_PKTINFO)

-- | The identifier for 'IPv6PktInfo'.
pattern CmsgIdIPv6PktInfo :: CmsgId
pattern CmsgIdIPv6PktInfo = CmsgId (#const IPPROTO_IPV6) (#const IPV6_PKTINFO)

-- Use WSADuplicateSocket for CmsgIdFd
-- pattern CmsgIdFd :: CmsgId

----------------------------------------------------------------

-- | Looking up control message. The following shows an example usage:
--
-- > (lookupCmsg CmsgIdIPv4TOS cmsgs >>= decodeCmsg) :: Maybe IPv4TOS
lookupCmsg :: CmsgId -> [Cmsg] -> Maybe Cmsg
lookupCmsg _   [] = Nothing
lookupCmsg cid (cmsg:cmsgs)
  | cmsgId cmsg == cid = Just cmsg
  | otherwise          = lookupCmsg cid cmsgs

-- | Filtering control message.
filterCmsg :: CmsgId -> [Cmsg] -> [Cmsg]
filterCmsg cid cmsgs = filter (\cmsg -> cmsgId cmsg == cid) cmsgs

----------------------------------------------------------------

-- | A class to encode and decode control message.
class Storable a => ControlMessage a where
    controlMessageId :: a -> CmsgId

encodeCmsg :: ControlMessage a => a -> Cmsg
encodeCmsg x = unsafeDupablePerformIO $ do
    bs <- create siz $ \p0 -> do
        let p = castPtr p0
        poke p x
    return $ Cmsg (controlMessageId x) bs
  where
    siz = sizeOf x

decodeCmsg :: forall a . Storable a => Cmsg -> Maybe a
decodeCmsg (Cmsg _ (PS fptr off len))
  | len < siz = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off)
        Just <$> peek p
  where
    siz = sizeOf (undefined :: a)

----------------------------------------------------------------

-- | Time to live of IPv4.
newtype IPv4TTL = IPv4TTL DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv4TTL where
    controlMessageId _ = CmsgIdIPv4TTL

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv6HopLimit where
    controlMessageId _ = CmsgIdIPv6HopLimit

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv4TOS where
    controlMessageId _ = CmsgIdIPv4TOS

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv6TClass where
    controlMessageId _ = CmsgIdIPv6TClass

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv4PktInfo = IPv4PktInfo ULONG HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple ha)

instance ControlMessage IPv4PktInfo where
    controlMessageId _ = CmsgIdIPv4PktInfo

instance Storable IPv4PktInfo where
    sizeOf      = const #{size IN_PKTINFO}
    alignment _ = #alignment IN_PKTINFO
    poke p (IPv4PktInfo n ha) = do
        (#poke IN_PKTINFO, ipi_ifindex)  p (fromIntegral n :: CInt)
        (#poke IN_PKTINFO, ipi_addr)     p ha
    peek p = do
        n  <- (#peek IN_PKTINFO, ipi_ifindex)  p
        ha <- (#peek IN_PKTINFO, ipi_addr)     p
        return $ IPv4PktInfo n ha

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv6PktInfo = IPv6PktInfo Int HostAddress6 deriving (Eq)

instance Show IPv6PktInfo where
    show (IPv6PktInfo n ha6) = "IPv6PktInfo " ++ show n ++ " " ++ show (hostAddress6ToTuple ha6)

instance ControlMessage IPv6PktInfo where
    controlMessageId _ = CmsgIdIPv6PktInfo

instance Storable IPv6PktInfo where
    sizeOf      = const #{size IN6_PKTINFO}
    alignment _ = #alignment IN6_PKTINFO
    poke p (IPv6PktInfo n ha6) = do
        (#poke IN6_PKTINFO, ipi6_ifindex) p (fromIntegral n :: CInt)
        (#poke IN6_PKTINFO, ipi6_addr)    p (In6Addr ha6)
    peek p = do
        In6Addr ha6 <- (#peek IN6_PKTINFO, ipi6_addr)    p
        n :: ULONG  <- (#peek IN6_PKTINFO, ipi6_ifindex) p
        return $ IPv6PktInfo (fromIntegral n) ha6
