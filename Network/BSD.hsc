{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.BSD
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The "Network.BSD" module defines Haskell bindings to network
-- programming functionality provided by BSD Unix derivatives.
--
-----------------------------------------------------------------------------

#include "HsNet.h"

-- NOTE: ##, we want this interpreted when compiling the .hs, not by hsc2hs.
##include "Typeable.h"

module Network.BSD (
       
    -- * Host names
    HostName,
    getHostName,	    -- :: IO HostName

    HostEntry(..),
    getHostByName,	    -- :: HostName -> IO HostEntry
    getHostByAddr,	    -- :: HostAddress -> Family -> IO HostEntry
    hostAddress,	    -- :: HostEntry -> HostAddress

#if defined(HAVE_GETHOSTENT) && !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
    getHostEntries,	    -- :: Bool -> IO [HostEntry]
    -- ** Low level functionality
    setHostEntry,	    -- :: Bool -> IO ()
    getHostEntry,	    -- :: IO HostEntry
    endHostEntry,	    -- :: IO ()
#endif

    -- * Service names
    ServiceEntry(..),
    ServiceName,
    getServiceByName,	    -- :: ServiceName -> ProtocolName -> IO ServiceEntry
    getServiceByPort,       -- :: PortNumber  -> ProtocolName -> IO ServiceEntry
    getServicePortNumber,   -- :: ServiceName -> IO PortNumber

#if !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
    getServiceEntries,	    -- :: Bool -> IO [ServiceEntry]
    -- ** Low level functionality
    getServiceEntry,	    -- :: IO ServiceEntry
    setServiceEntry,	    -- :: Bool -> IO ()
    endServiceEntry,	    -- :: IO ()
#endif

    -- * Protocol names
    ProtocolName,
    ProtocolNumber,
    ProtocolEntry(..),
    getProtocolByName,	    -- :: ProtocolName   -> IO ProtocolEntry
    getProtocolByNumber,    -- :: ProtocolNumber -> IO ProtcolEntry
    getProtocolNumber,	    -- :: ProtocolName   -> ProtocolNumber
    defaultProtocol,        -- :: ProtocolNumber

#if !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
    getProtocolEntries,	    -- :: Bool -> IO [ProtocolEntry]
    -- ** Low level functionality
    setProtocolEntry,	    -- :: Bool -> IO ()
    getProtocolEntry,	    -- :: IO ProtocolEntry
    endProtocolEntry,	    -- :: IO ()
#endif

    -- * Port numbers
    PortNumber,

    -- * Network names
    NetworkName,
    NetworkAddr,
    NetworkEntry(..)

#if !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
    , getNetworkByName	    -- :: NetworkName -> IO NetworkEntry
    , getNetworkByAddr      -- :: NetworkAddr -> Family -> IO NetworkEntry
    , getNetworkEntries     -- :: Bool -> IO [NetworkEntry]
    -- ** Low level functionality
    , setNetworkEntry	    -- :: Bool -> IO ()
    , getNetworkEntry	    -- :: IO NetworkEntry
    , endNetworkEntry	    -- :: IO ()
#endif
    ) where

#ifdef __HUGS__
import Hugs.Prelude ( IOException(..), IOErrorType(..) )
#endif
import Network.Socket

import Control.Concurrent 	( MVar, newMVar, withMVar )
import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_ )
import Foreign.C.String ( CString, peekCString, peekCStringLen, withCString )
import Foreign.C.Types ( CInt, CULong, CChar, CSize, CShort )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Storable ( Storable(..) )
import Foreign.Marshal.Array ( allocaArray0, peekArray0 )
import Foreign.Marshal.Utils ( with, fromBool )
import Data.Typeable
import System.IO.Unsafe ( unsafePerformIO )

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif

import Control.Monad ( liftM )

-- ---------------------------------------------------------------------------
-- Basic Types

type HostName     = String
type ProtocolName = String
type ServiceName  = String

-- ---------------------------------------------------------------------------
-- Service Database Access

-- Calling getServiceByName for a given service and protocol returns
-- the systems service entry.  This should be used to find the port
-- numbers for standard protocols such as SMTP and FTP.  The remaining
-- three functions should be used for browsing the service database
-- sequentially.

-- Calling setServiceEntry with True indicates that the service
-- database should be left open between calls to getServiceEntry.  To
-- close the database a call to endServiceEntry is required.  This
-- database file is usually stored in the file /etc/services.

data ServiceEntry  = 
  ServiceEntry  {
     serviceName     :: ServiceName,	-- Official Name
     serviceAliases  :: [ServiceName],	-- aliases
     servicePort     :: PortNumber,	-- Port Number  ( network byte order )
     serviceProtocol :: ProtocolName	-- Protocol
  } deriving (Show)

INSTANCE_TYPEABLE0(ServiceEntry,serviceEntryTc,"ServiceEntry")

instance Storable ServiceEntry where
   sizeOf    _ = #const sizeof(struct servent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        s_name    <- (#peek struct servent, s_name) p >>= peekCString
        s_aliases <- (#peek struct servent, s_aliases) p
                           >>= peekArray0 nullPtr
                           >>= mapM peekCString
        s_port    <- (#peek struct servent, s_port) p
        s_proto   <- (#peek struct servent, s_proto) p >>= peekCString
        return (ServiceEntry {
                        serviceName     = s_name,
                        serviceAliases  = s_aliases,
#if defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS)
                        servicePort     = PortNum (fromIntegral (s_port :: CShort)),
#else
                           -- s_port is already in network byte order, but it
                           -- might be the wrong size.
                        servicePort     = PortNum (fromIntegral (s_port :: CInt)),
#endif
                        serviceProtocol = s_proto
                })

   poke p = error "Storable.poke(BSD.ServiceEntry) not implemented"


-- | Get service by name.
getServiceByName :: ServiceName 	-- Service Name
		 -> ProtocolName 	-- Protocol Name
		 -> IO ServiceEntry	-- Service Entry
getServiceByName name proto = withLock $ do
 withCString name  $ \ cstr_name  -> do
 withCString proto $ \ cstr_proto -> do
 throwNoSuchThingIfNull "getServiceByName" "no such service entry"
   $ (trySysCall (c_getservbyname cstr_name cstr_proto))
 >>= peek

foreign import ccall unsafe "getservbyname" 
  c_getservbyname :: CString -> CString -> IO (Ptr ServiceEntry)

-- | Get the service given a 'PortNumber' and 'ProtocolName'.
getServiceByPort :: PortNumber -> ProtocolName -> IO ServiceEntry
getServiceByPort (PortNum port) proto = withLock $ do
 withCString proto $ \ cstr_proto -> do
 throwNoSuchThingIfNull "getServiceByPort" "no such service entry"
   $ (trySysCall (c_getservbyport (fromIntegral port) cstr_proto))
 >>= peek

foreign import ccall unsafe "getservbyport" 
  c_getservbyport :: CInt -> CString -> IO (Ptr ServiceEntry)

-- | Get the 'PortNumber' corresponding to the 'ServiceName'.
getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber name = do
    (ServiceEntry _ _ port _) <- getServiceByName name "tcp"
    return port

#if !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
getServiceEntry	:: IO ServiceEntry
getServiceEntry = withLock $ do
 throwNoSuchThingIfNull "getServiceEntry" "no such service entry"
   $ trySysCall c_getservent
 >>= peek

foreign import ccall unsafe "getservent" c_getservent :: IO (Ptr ServiceEntry)

setServiceEntry	:: Bool -> IO ()
setServiceEntry flg = withLock $ trySysCall $ c_setservent (fromBool flg)

foreign import ccall unsafe  "setservent" c_setservent :: CInt -> IO ()

endServiceEntry	:: IO ()
endServiceEntry = withLock $ trySysCall $ c_endservent

foreign import ccall unsafe  "endservent" c_endservent :: IO ()

getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries stayOpen = do
  setServiceEntry stayOpen
  getEntries (getServiceEntry) (endServiceEntry)
#endif

-- ---------------------------------------------------------------------------
-- Protocol Entries

-- The following relate directly to the corresponding UNIX C
-- calls for returning the protocol entries. The protocol entry is
-- represented by the Haskell type ProtocolEntry.

-- As for setServiceEntry above, calling setProtocolEntry.
-- determines whether or not the protocol database file, usually
-- @/etc/protocols@, is to be kept open between calls of
-- getProtocolEntry. Similarly, 

data ProtocolEntry = 
  ProtocolEntry  {
     protoName    :: ProtocolName,	-- Official Name
     protoAliases :: [ProtocolName],	-- aliases
     protoNumber  :: ProtocolNumber	-- Protocol Number
  } deriving (Read, Show)

INSTANCE_TYPEABLE0(ProtocolEntry,protocolEntryTc,"ProtocolEntry")

instance Storable ProtocolEntry where
   sizeOf    _ = #const sizeof(struct protoent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        p_name    <- (#peek struct protoent, p_name) p >>= peekCString
        p_aliases <- (#peek struct protoent, p_aliases) p
                           >>= peekArray0 nullPtr
                           >>= mapM peekCString
#if defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS)
         -- With WinSock, the protocol number is only a short;
         -- hoist it in as such, but represent it on the Haskell side
         -- as a CInt.
        p_proto_short  <- (#peek struct protoent, p_proto) p 
        let p_proto = fromIntegral (p_proto_short :: CShort)
#else
        p_proto        <- (#peek struct protoent, p_proto) p 
#endif
        return (ProtocolEntry { 
                        protoName    = p_name,
                        protoAliases = p_aliases,
                        protoNumber  = p_proto
                })

   poke p = error "Storable.poke(BSD.ProtocolEntry) not implemented"

getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName name = withLock $ do
 withCString name $ \ name_cstr -> do
 throwNoSuchThingIfNull "getProtocolByName" ("no such protocol name: " ++ name)
   $ (trySysCall.c_getprotobyname) name_cstr
 >>= peek

foreign import  ccall unsafe  "getprotobyname" 
   c_getprotobyname :: CString -> IO (Ptr ProtocolEntry)


getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber num = withLock $ do
 throwNoSuchThingIfNull "getProtocolByNumber" ("no such protocol number: " ++ show num)
   $ (trySysCall.c_getprotobynumber) (fromIntegral num)
 >>= peek

foreign import ccall unsafe  "getprotobynumber"
   c_getprotobynumber :: CInt -> IO (Ptr ProtocolEntry)


getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber proto = do
 (ProtocolEntry _ _ num) <- getProtocolByName proto
 return num

-- | This is the default protocol for the given service.
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

#if !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
getProtocolEntry :: IO ProtocolEntry	-- Next Protocol Entry from DB
getProtocolEntry = withLock $ do
 ent <- throwNoSuchThingIfNull "getProtocolEntry" "no such protocol entry"
   		$ trySysCall c_getprotoent
 peek ent

foreign import ccall unsafe  "getprotoent" c_getprotoent :: IO (Ptr ProtocolEntry)

setProtocolEntry :: Bool -> IO ()	-- Keep DB Open ?
setProtocolEntry flg = withLock $ trySysCall $ c_setprotoent (fromBool flg)

foreign import ccall unsafe "setprotoent" c_setprotoent :: CInt -> IO ()

endProtocolEntry :: IO ()
endProtocolEntry = withLock $ trySysCall $ c_endprotoent

foreign import ccall unsafe "endprotoent" c_endprotoent :: IO ()

getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries stayOpen = withLock $ do
  setProtocolEntry stayOpen
  getEntries (getProtocolEntry) (endProtocolEntry)
#endif

-- ---------------------------------------------------------------------------
-- Host lookups

data HostEntry = 
  HostEntry  {
     hostName      :: HostName,  	-- Official Name
     hostAliases   :: [HostName],	-- aliases
     hostFamily    :: Family,	        -- Host Type (currently AF_INET)
     hostAddresses :: [HostAddress]	-- Set of Network Addresses  (in network byte order)
  } deriving (Read, Show)

INSTANCE_TYPEABLE0(HostEntry,hostEntryTc,"hostEntry")

instance Storable HostEntry where
   sizeOf    _ = #const sizeof(struct hostent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        h_name       <- (#peek struct hostent, h_name) p >>= peekCString
        h_aliases    <- (#peek struct hostent, h_aliases) p
                                >>= peekArray0 nullPtr
                                >>= mapM peekCString
        h_addrtype   <- (#peek struct hostent, h_addrtype) p
        -- h_length       <- (#peek struct hostent, h_length) p
        h_addr_list  <- (#peek struct hostent, h_addr_list) p
                                >>= peekArray0 nullPtr
                                >>= mapM peek
        return (HostEntry {
                        hostName       = h_name,
                        hostAliases    = h_aliases,
                        hostFamily     = unpackFamily h_addrtype,
                        hostAddresses  = h_addr_list
                })

   poke p = error "Storable.poke(BSD.ServiceEntry) not implemented"


-- convenience function:
hostAddress :: HostEntry -> HostAddress
hostAddress (HostEntry nm _ _ ls) =
 case ls of
   []    -> error ("BSD.hostAddress: empty network address list for " ++ nm)
   (x:_) -> x

-- getHostByName must use the same lock as the *hostent functions
-- may cause problems if called concurrently.

-- | Resolve a 'HostName' to IPv4 address.
getHostByName :: HostName -> IO HostEntry
getHostByName name = withLock $ do
  withCString name $ \ name_cstr -> do
   ent <- throwNoSuchThingIfNull "getHostByName" "no such host entry"
    		$ trySysCall $ c_gethostbyname name_cstr
   peek ent

foreign import ccall safe "gethostbyname" 
   c_gethostbyname :: CString -> IO (Ptr HostEntry)


-- The locking of gethostbyaddr is similar to gethostbyname.
-- | Get a 'HostEntry' corresponding to the given address and family.
-- Note that only IPv4 is currently supported.
getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr family addr = do
 with addr $ \ ptr_addr -> withLock $ do
 throwNoSuchThingIfNull 	"getHostByAddr" "no such host entry"
   $ trySysCall $ c_gethostbyaddr ptr_addr (fromIntegral (sizeOf addr)) (packFamily family)
 >>= peek

foreign import ccall safe "gethostbyaddr"
   c_gethostbyaddr :: Ptr HostAddress -> CInt -> CInt -> IO (Ptr HostEntry)

#if defined(HAVE_GETHOSTENT) && !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
getHostEntry :: IO HostEntry
getHostEntry = withLock $ do
 throwNoSuchThingIfNull 	"getHostEntry" "unable to retrieve host entry"
   $ trySysCall $ c_gethostent
 >>= peek

foreign import ccall unsafe "gethostent" c_gethostent :: IO (Ptr HostEntry)

setHostEntry :: Bool -> IO ()
setHostEntry flg = withLock $ trySysCall $ c_sethostent (fromBool flg)

foreign import ccall unsafe "sethostent" c_sethostent :: CInt -> IO ()

endHostEntry :: IO ()
endHostEntry = withLock $ c_endhostent

foreign import ccall unsafe "endhostent" c_endhostent :: IO ()

getHostEntries :: Bool -> IO [HostEntry]
getHostEntries stayOpen = do
  setHostEntry stayOpen
  getEntries (getHostEntry) (endHostEntry)
#endif

-- ---------------------------------------------------------------------------
-- Accessing network information

-- Same set of access functions as for accessing host,protocol and
-- service system info, this time for the types of networks supported.

-- network addresses are represented in host byte order.
type NetworkAddr = CULong

type NetworkName = String

data NetworkEntry =
  NetworkEntry {
     networkName	:: NetworkName,   -- official name
     networkAliases	:: [NetworkName], -- aliases
     networkFamily	:: Family,	   -- type
     networkAddress	:: NetworkAddr
   } deriving (Read, Show)

INSTANCE_TYPEABLE0(NetworkEntry,networkEntryTc,"NetworkEntry")

instance Storable NetworkEntry where
   sizeOf    _ = #const sizeof(struct hostent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        n_name         <- (#peek struct netent, n_name) p >>= peekCString
        n_aliases      <- (#peek struct netent, n_aliases) p
                                >>= peekArray0 nullPtr
                                >>= mapM peekCString
        n_addrtype     <- (#peek struct netent, n_addrtype) p
        n_net          <- (#peek struct netent, n_net) p
        return (NetworkEntry {
                        networkName      = n_name,
                        networkAliases   = n_aliases,
                        networkFamily    = unpackFamily (fromIntegral 
                                                        (n_addrtype :: CInt)),
                        networkAddress   = n_net
                })

   poke p = error "Storable.poke(BSD.NetEntry) not implemented"


#if !defined(cygwin32_HOST_OS) && !defined(mingw32_HOST_OS) && !defined(_WIN32)
getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName name = withLock $ do
 withCString name $ \ name_cstr -> do
  throwNoSuchThingIfNull "getNetworkByName" "no such network entry"
    $ trySysCall $ c_getnetbyname name_cstr
  >>= peek

foreign import ccall unsafe "getnetbyname" 
   c_getnetbyname  :: CString -> IO (Ptr NetworkEntry)

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr addr family = withLock $ do
 throwNoSuchThingIfNull "getNetworkByAddr" "no such network entry"
   $ trySysCall $ c_getnetbyaddr addr (packFamily family)
 >>= peek

foreign import ccall unsafe "getnetbyaddr" 
   c_getnetbyaddr  :: NetworkAddr -> CInt -> IO (Ptr NetworkEntry)

getNetworkEntry :: IO NetworkEntry
getNetworkEntry = withLock $ do
 throwNoSuchThingIfNull "getNetworkEntry" "no more network entries"
          $ trySysCall $ c_getnetent
 >>= peek

foreign import ccall unsafe "getnetent" c_getnetent :: IO (Ptr NetworkEntry)

-- | Open the network name database. The parameter specifies
-- whether a connection is maintained open between various
-- networkEntry calls
setNetworkEntry :: Bool -> IO ()
setNetworkEntry flg = withLock $ trySysCall $ c_setnetent (fromBool flg)

foreign import ccall unsafe "setnetent" c_setnetent :: CInt -> IO ()

-- | Close the connection to the network name database.
endNetworkEntry :: IO ()
endNetworkEntry = withLock $ trySysCall $ c_endnetent

foreign import ccall unsafe "endnetent" c_endnetent :: IO ()

-- | Get the list of network entries.
getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries stayOpen = do
  setNetworkEntry stayOpen
  getEntries (getNetworkEntry) (endNetworkEntry)
#endif

-- Mutex for name service lockdown

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

withLock :: IO a -> IO a
withLock act = withMVar lock (\_ -> act)

-- ---------------------------------------------------------------------------
-- Miscellaneous Functions

-- | Calling getHostName returns the standard host name for the current
-- processor, as set at boot time.

getHostName :: IO HostName
getHostName = do
  let size = 256
  allocaArray0 size $ \ cstr -> do
    throwSocketErrorIfMinus1_ "getHostName" $ c_gethostname cstr (fromIntegral size)
    peekCString cstr

foreign import ccall unsafe "gethostname" 
   c_gethostname :: CString -> CSize -> IO CInt

-- Helper function used by the exported functions that provides a
-- Haskellised view of the enumerator functions:

getEntries :: IO a  -- read
           -> IO () -- at end
	   -> IO [a]
getEntries getOne atEnd = loop
  where
    loop = do
      vv <- catch (liftM Just getOne) ((const.return) Nothing)
      case vv of
        Nothing -> return []
        Just v  -> loop >>= \ vs -> atEnd >> return (v:vs)


-- ---------------------------------------------------------------------------
-- Winsock only:
--   The BSD API networking calls made locally return NULL upon failure.
--   That failure may very well be due to WinSock not being initialised,
--   so if NULL is seen try init'ing and repeat the call.
#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
trySysCall act = act
#else
trySysCall act = do
  ptr <- act
  if (ptr == nullPtr)
   then withSocketsDo act
   else return ptr
#endif

throwNoSuchThingIfNull :: String -> String -> IO (Ptr a) -> IO (Ptr a)
throwNoSuchThingIfNull loc desc act = do
  ptr <- act
  if (ptr == nullPtr)
   then ioError (IOError Nothing NoSuchThing loc desc Nothing)
   else return ptr
