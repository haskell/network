-----------------------------------------------------------------------------
-- 
-- Module      :  Network
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- $Id: Network.hs,v 1.1 2001/08/01 13:33:26 simonmar Exp $
--
-- Basic network interface
--
-----------------------------------------------------------------------------

module Network (
	Socket,
        PortID(..),
	Hostname,

	connectTo,	-- :: Hostname -> PortID -> IO Handle
	listenOn,	-- :: PortID -> IO Socket
	
	accept,		-- :: Socket -> IO (Handle, HostName, PortNumber)

	sendTo,		-- :: Hostname -> PortID -> String -> IO ()
	recvFrom,	-- :: Hostname -> PortID -> IO String

	socketPort,	-- :: Socket -> IO PortID
	
	withSocketsDo,  -- :: IO a   -> IO a
	
	PortNumber,	-- instance (Eq, Enum, Num, Real, Integral)

       ) where

#include "config.h"

import Network.BSD
import Network.Socket hiding ( accept, socketPort, recvFrom, sendTo, PortNumber )
import qualified Network.Socket as Socket ( accept, socketPort )
import System.IO

-- ---------------------------------------------------------------------------
-- High Level ``Setup'' functions

-- Calling @connectTo@ creates a client side socket which is
-- connected to the given host and port.  The Protocol and socket type is
-- derived from the given port identifier.  If a port number is given
-- then the result is always an internet family @Stream@ socket. 

-- If the @PortID@ specifies a unix family socket and the @Hostname@
-- differs from that returned by @getHostname@ then an error is
-- raised. Alternatively an empty string may be given to @connectTo@
-- signalling that the current hostname applies.

data PortID = 
	  Service String		-- Service Name eg "ftp"
	| PortNumber PortNumber		-- User defined Port Number
#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS)
	| UnixSocket String		-- Unix family socket in file system
#endif

type Hostname = String
-- Maybe consider this alternative.
-- data Hostname = Name String | IP Int Int Int Int

connectTo :: Hostname		-- Hostname
	  -> PortID 		-- Port Identifier
	  -> IO Handle		-- Connected Socket

connectTo hostname (Service serv) = do
    proto	<- getProtocolNumber "tcp"
    sock	<- socket AF_INET Stream proto
    port	<- getServicePortNumber serv
    he		<- getHostByName hostname
    connect sock (SockAddrInet port (hostAddress he))
    socketToHandle sock	ReadWriteMode

connectTo hostname (PortNumber port) = do
    proto	<- getProtocolNumber "tcp"
    sock        <- socket AF_INET Stream proto
    he		<- getHostByName hostname
    connect sock (SockAddrInet port (hostAddress he))
    socketToHandle sock ReadWriteMode

#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS)
connectTo _ (UnixSocket path) = do
    sock    <- socket AF_UNIX Datagram 0
    connect sock (SockAddrUnix path)
    socketToHandle sock ReadWriteMode
#endif

-- The dual to the @connectTo@ call. This creates the server side
-- socket which has been bound to the specified port.
-- NOTE: To avoid the "Address already in use" problems popped up
-- several times in GHC-Users we set ReuseAddr to 1.

listenOn :: PortID 	-- Port Identifier
	 -> IO Socket	-- Connected Socket

listenOn (Service serv) = do
    proto   <- getProtocolNumber "tcp"
    sock    <- socket AF_INET Stream proto
    port    <- getServicePortNumber serv
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock maxListenQueue
    return sock

listenOn (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    sock  <- socket AF_INET Stream proto
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock maxListenQueue
    return sock

#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS)
listenOn (UnixSocket path) = do
    sock <- socket AF_UNIX Datagram 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrUnix path)
    return sock
#endif

-- -----------------------------------------------------------------------------
-- accept

accept :: Socket 		-- Listening Socket
       -> IO (Handle, 		-- StdIO Handle for read/write
	      HostName, 	-- HostName of Peer socket
	      PortNumber)	-- Portnumber of remote connection
accept sock = do
 ~(sock', (SockAddrInet port haddr)) <- Socket.accept sock
 (HostEntry peer _ _ _)           <- getHostByAddr AF_INET haddr
 handle				  <- socketToHandle sock' ReadWriteMode
 return (handle, peer, port)

-- -----------------------------------------------------------------------------
-- sendTo/recvFrom

-- Send and recived data from/to the given host and port number.  These
-- should normally only be used where the socket will not be required for
-- further calls.

sendTo :: Hostname 	-- Hostname
       -> PortID	-- Port Number
       -> String	-- Message to send
       -> IO ()
sendTo h p msg = do
  s <- connectTo h p
  hPutStr s msg
  hClose s

recvFrom :: Hostname 	-- Hostname
	 -> PortID	-- Port Number
	 -> IO String	-- Received Data
recvFrom host port = do
 s <- listenOn port
 let 
  waiting = do
     ~(s', SockAddrInet _ haddr) <-  Socket.accept s
     (HostEntry peer _ _ _)      <- getHostByAddr AF_INET haddr
     if peer /= host 
      then do
         sClose s'
         waiting
      else do
	h <- socketToHandle s' ReadMode
        msg <- hGetContents h
        hClose h
        return msg

 message <- waiting
 sClose s
 return message

-- ---------------------------------------------------------------------------
-- Access function returning the port type/id of socket.

socketPort :: Socket -> IO PortID
socketPort s = do
    sockaddr <- getSocketName s
    return (portID sockaddr)
  where
   portID sa =
    case sa of
     SockAddrInet port _    -> PortNumber port
#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS)
     SockAddrUnix path	    -> UnixSocket path
#endif
