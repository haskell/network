{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Prelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/net/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Misc BSD bindings
--
-- The BSD module defines Haskell bindings to functionality
-- provided by BSD Unix derivatives. Currently this covers
-- network programming functionality and symbolic links.
-- (OK, so the latter is pretty much supported by most *nixes
-- today, but it was BSD that introduced them.)
--
-----------------------------------------------------------------------------

#include "config.h"

module Network.BSD (
       
    HostName,
    getHostName,	    -- :: IO HostName

    ServiceEntry(..),
    ServiceName,
    getServiceByName,	    -- :: ServiceName -> ProtocolName -> IO ServiceEntry
    getServiceByPort,       -- :: PortNumber  -> ProtocolName -> IO ServiceEntry
    getServicePortNumber,   -- :: ServiceName -> IO PortNumber

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
    getServiceEntry,	    -- :: IO ServiceEntry
    setServiceEntry,	    -- :: Bool -> IO ()
    endServiceEntry,	    -- :: IO ()
    getServiceEntries,	    -- :: Bool -> IO [ServiceEntry]
#endif

    ProtocolName,
    ProtocolNumber,
    ProtocolEntry(..),
    getProtocolByName,	    -- :: ProtocolName   -> IO ProtocolEntry
    getProtocolByNumber,    -- :: ProtocolNumber -> IO ProtcolEntry
    getProtocolNumber,	    -- :: ProtocolName   -> ProtocolNumber

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
    setProtocolEntry,	    -- :: Bool -> IO ()
    getProtocolEntry,	    -- :: IO ProtocolEntry
    endProtocolEntry,	    -- :: IO ()
    getProtocolEntries,	    -- :: Bool -> IO [ProtocolEntry]
#endif

    PortNumber,

    HostEntry(..),
    getHostByName,	    -- :: HostName -> IO HostEntry
    getHostByAddr,	    -- :: HostAddress -> Family -> IO HostEntry
    hostAddress,	    -- :: HostEntry -> HostAddress

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
    setHostEntry,	    -- :: Bool -> IO ()
    getHostEntry,	    -- :: IO HostEntry
    endHostEntry,	    -- :: IO ()
    getHostEntries,	    -- :: Bool -> IO [HostEntry]
#endif

    NetworkName,
    NetworkAddr,
    NetworkEntry(..),

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
    getNetworkByName,	    -- :: NetworkName -> IO NetworkEntry
    getNetworkByAddr,       -- :: NetworkAddr -> Family -> IO NetworkEntry
    setNetworkEntry,	    -- :: Bool -> IO ()
    getNetworkEntry,	    -- :: IO NetworkEntry
    endNetworkEntry,	    -- :: IO ()
    getNetworkEntries,      -- :: Bool -> IO [NetworkEntry]
#endif

#ifdef HAVE_SYMLINK
    , symlink		    -- :: String -> String -> IO ()
#endif
#ifdef HAVE_READLINK
    , readlink		    -- :: String -> IO String
#endif

    ) where

#include "HsNet.h"

import Network.Socket

import Foreign.C
import Foreign

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif

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
  }

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
			   -- s_port is already in network byte order, but it
			   -- might be the wrong size.
			servicePort     = PortNum (fromIntegral (s_port :: CInt)),
			serviceProtocol = s_proto
		})

   poke p = error "Storable.poke(BSD.ServiceEntry) not implemented"


getServiceByName :: ServiceName 	-- Service Name
		 -> ProtocolName 	-- Protocol Name
		 -> IO ServiceEntry	-- Service Entry
getServiceByName name proto = do
 withCString name  $ \ cstr_name  -> do
 withCString proto $ \ cstr_proto -> do
 ptr <- c_getservbyname cstr_name cstr_proto
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getServiceByName" "no such service entry" Nothing)
    else peek ptr

foreign import ccall unsafe "getservbyname" 
  c_getservbyname :: CString -> CString -> IO (Ptr ServiceEntry)

getServiceByPort :: PortNumber -> ProtocolName -> IO ServiceEntry
getServiceByPort (PortNum port) proto = do
 withCString proto $ \ cstr_proto -> do
 ptr <- c_getservbyport (fromIntegral port) cstr_proto
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	  "getServiceByPort" "no such service entry" Nothing)
    else peek ptr

foreign import ccall unsafe "getservbyport" 
  c_getservbyport :: CInt -> CString -> IO (Ptr ServiceEntry)

getServicePortNumber :: ServiceName -> IO PortNumber
getServicePortNumber name = do
    (ServiceEntry _ _ port _) <- getServiceByName name "tcp"
    return port

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
getServiceEntry	:: IO ServiceEntry
getServiceEntry = do
    ptr <- c_getservent
    if ptr == nullPtr
       then ioException (IOError Nothing NoSuchThing
	   "getServiceEntry" "no such service entry" Nothing)
       else peek ptr

foreign import ccall unsafe "getservent" c_getservent :: IO (Ptr ServiceEntry)

setServiceEntry	:: Bool -> IO ()
setServiceEntry flg = c_setservent (fromBool flg)

foreign import ccall unsafe  "setservent" c_setservent :: CInt -> IO ()

endServiceEntry	:: IO ()
endServiceEntry = c_endservent

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
  }

instance Storable ProtocolEntry where
   sizeOf    _ = #const sizeof(struct protoent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
	p_name    <- (#peek struct protoent, p_name) p >>= peekCString
	p_aliases <- (#peek struct protoent, p_aliases) p
			   >>= peekArray0 nullPtr
			   >>= mapM peekCString
	p_proto        <- (#peek struct protoent, p_proto) p 
	return (ProtocolEntry { 
			protoName    = p_name,
			protoAliases = p_aliases,
			protoNumber  = p_proto
		})

   poke p = error "Storable.poke(BSD.ProtocolEntry) not implemented"

getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName name = do
 withCString name $ \ name_cstr -> do
 ptr <- c_getprotobyname name_cstr
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getProtocolByName" "no such protocol entry" Nothing)
    else peek ptr

foreign import  ccall unsafe  "getprotobyname" 
   c_getprotobyname :: CString -> IO (Ptr ProtocolEntry)


getProtocolByNumber :: ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber num = do
 ptr <- c_getprotobynumber (fromIntegral num)
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getProtocolByNumber" "no such protocol entry" Nothing)
    else peek ptr

foreign import ccall unsafe  "getprotobynumber"
   c_getprotobynumber :: CInt -> IO (Ptr ProtocolEntry)


getProtocolNumber :: ProtocolName -> IO ProtocolNumber
getProtocolNumber proto = do
 (ProtocolEntry _ _ num) <- getProtocolByName proto
 return num

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
getProtocolEntry :: IO ProtocolEntry	-- Next Protocol Entry from DB
getProtocolEntry = do
 ptr <- c_getprotoent
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getProtocolEntry" "no such protocol entry" Nothing)
    else peek ptr

foreign import ccall unsafe  "getprotoent" c_getprotoent :: IO (Ptr ProtocolEntry)

setProtocolEntry :: Bool -> IO ()	-- Keep DB Open ?
setProtocolEntry flg = c_setprotoent (fromBool flg)

foreign import ccall unsafe "setprotoent" c_setprotoent :: CInt -> IO ()

endProtocolEntry :: IO ()
endProtocolEntry = c_endprotoent

foreign import ccall unsafe "endprotoent" c_endprotoent :: IO ()

getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries stayOpen = do
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
  }

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

getHostByName :: HostName -> IO HostEntry
getHostByName name = do
  withCString name $ \ name_cstr -> do
  ptr <- c_gethostbyname name_cstr
  if ptr == nullPtr
     then ioException (IOError Nothing NoSuchThing
	   "getHostByName" "no such host entry" Nothing)
     else peek ptr

foreign import ccall unsafe "gethostbyname" 
   c_gethostbyname :: CString -> IO (Ptr HostEntry)

getHostByAddr :: Family -> HostAddress -> IO HostEntry
getHostByAddr family addr = do
 withObject addr $ \ ptr_addr -> do
 ptr <- c_gethostbyaddr ptr_addr (fromIntegral (sizeOf addr)) (packFamily family)
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getHostByAddr" "no such host entry" Nothing)
    else peek ptr

foreign import ccall unsafe "gethostbyaddr"
   c_gethostbyaddr :: Ptr HostAddress -> CInt -> CInt -> IO (Ptr HostEntry)

#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
getHostEntry :: IO HostEntry
getHostEntry = do
 ptr <- c_gethostent
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getHostEntry" "unable to retrieve host entry" Nothing)
    else peek ptr

foreign import ccall unsafe "gethostent" c_gethostent :: IO (Ptr HostEntry)

setHostEntry :: Bool -> IO ()
setHostEntry flg = c_sethostent (fromBool flg)

foreign import ccall unsafe "sethostent" c_sethostent :: CInt -> IO ()

endHostEntry :: IO ()
endHostEntry = c_endhostent

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
   }

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


#if !defined(cygwin32_TARGET_OS) && !defined(mingw32_TARGET_OS)
getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName name = do
 withCString name $ \ name_cstr -> do
 ptr <- c_getnetbyname name_cstr
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getNetworkByName" "no such network entry" Nothing)
    else peek ptr

foreign import ccall unsafe "getnetbyname" 
   c_getnetbyname  :: CString -> IO (Ptr NetworkEntry)

getNetworkByAddr :: NetworkAddr -> Family -> IO NetworkEntry
getNetworkByAddr addr family = do
 ptr <- c_getnetbyaddr addr (packFamily family)
 if ptr == nullPtr
    then ioException (IOError Nothing NoSuchThing
	"getNetworkByAddr" "no such network entry" Nothing)
    else peek ptr

foreign import ccall unsafe "getnetbyaddr" 
   c_getnetbyaddr  :: NetworkAddr -> CInt -> IO (Ptr NetworkEntry)

getNetworkEntry :: IO NetworkEntry
getNetworkEntry = do
 ptr <- c_getnetent
 if ptr == nullPtr
   then ioException (IOError Nothing NoSuchThing
	"getNetworkEntry" "no more network entries" Nothing)
   else peek ptr

foreign import ccall unsafe "getnetent" c_getnetent :: IO (Ptr NetworkEntry)

setNetworkEntry :: Bool -> IO ()
setNetworkEntry flg = c_setnetent (fromBool flg)

foreign import ccall unsafe "setnetent" c_setnetent :: CInt -> IO ()

endNetworkEntry :: IO ()
endNetworkEntry = c_endnetent

foreign import ccall unsafe "endnetent" c_endnetent :: IO ()

getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries stayOpen = do
  setNetworkEntry stayOpen
  getEntries (getNetworkEntry) (endNetworkEntry)
#endif

-- ---------------------------------------------------------------------------
-- Miscellaneous Functions

-- Calling getHostName returns the standard host name for the current
-- processor, as set at boot time.

getHostName :: IO HostName
getHostName = do
  let size = 256
  allocaArray0 size $ \ cstr -> do
    throwErrnoIfMinus1_ "getHostName" $ c_gethostname cstr (fromIntegral size)
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
   loop = 
     catch (do { v <- getOne; vs <- loop ; return (v:vs) })
           (\ _ -> do { atEnd; return [] } )


-- ---------------------------------------------------------------------------
-- Symbolic links

#ifdef HAVE_SYMLINK
symlink :: String -> String -> IO ()
symlink actual_path sym_path = do
   withCString actual_path $ \ actual_path_cstr -> do
   withCString sym_path $ \ sym_path_cstr -> do
   throwErrnoIfMinus1_ "symlink" $ c_symlink actual_path_cstr sym_path_cstr

foreign import ccall unsafe "symlink" 
   c_symlink :: CString -> CString -> IO CInt
#endif

#ifdef HAVE_READLINK
readlink :: String -> IO String
readlink sym = do
   withCString sym $ \ sym_cstr -> do
   allocaArray0 (#const PATH_MAX) $ \ buf -> do
   rc <- throwErrnoIfMinus1 "readlink" $ 
	    c_readlink sym_cstr buf (#const PATH_MAX)
   peekCStringLen (buf, fromIntegral rc)

foreign import ccall unsafe "readlink"
   c_readlink :: CString -> Ptr CChar -> CSize -> IO CInt
#endif
