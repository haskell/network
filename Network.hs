{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network" interface is a \"higher-level\" interface to
-- networking facilities, and it is recommended unless you need the
-- lower-level interface in "Network.Socket".
--
-----------------------------------------------------------------------------

module Network (

	-- * Basic data types
	Socket,
        PortID(..),
	HostName,
	PortNumber,	-- instance (Eq, Enum, Num, Real, Integral)

	-- * Initialisation
	withSocketsDo,  -- :: IO a   -> IO a
	
	-- * Server-side connections
	listenOn,	-- :: PortID -> IO Socket
	accept,		-- :: Socket -> IO (Handle, HostName, PortNumber)

	-- * Client-side connections
	connectTo,	-- :: HostName -> PortID -> IO Handle

	-- * Simple sending and receiving
	sendTo,		-- :: HostName -> PortID -> String -> IO ()
	recvFrom,	-- :: HostName -> PortID -> IO String

	-- * Miscellaneous
	socketPort,	-- :: Socket -> IO PortID

	-- * Networking Issues
	-- ** Buffering
	{-$buffering-}

	-- ** Improving I\/O Performance over sockets
	{-$performance-}

	-- ** @SIGPIPE@
	{-$sigpipe-}

       ) where

#include "config.h"

import Network.BSD
import Network.Socket hiding ( accept, socketPort, recvFrom, sendTo, PortNumber )
import qualified Network.Socket as Socket ( accept )
import System.IO
import Prelude
import Control.Exception as Exception

-- ---------------------------------------------------------------------------
-- High Level ``Setup'' functions

-- If the @PortID@ specifies a unix family socket and the @Hostname@
-- differs from that returned by @getHostname@ then an error is
-- raised. Alternatively an empty string may be given to @connectTo@
-- signalling that the current hostname applies.

data PortID = 
	  Service String		-- Service Name eg "ftp"
	| PortNumber PortNumber		-- User defined Port Number
#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS) && !defined(_WIN32)
	| UnixSocket String		-- Unix family socket in file system
#endif

-- | Calling 'connectTo' creates a client side socket which is
-- connected to the given host and port.  The Protocol and socket type is
-- derived from the given port identifier.  If a port number is given
-- then the result is always an internet family 'Stream' socket. 

connectTo :: HostName		-- Hostname
	  -> PortID 		-- Port Identifier
	  -> IO Handle		-- Connected Socket

connectTo hostname (Service serv) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
	(socket AF_INET Stream proto)
	(sClose)  -- only done if there's an error
	(\sock -> do
          port	<- getServicePortNumber serv
          he	<- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          socketToHandle sock ReadWriteMode
 	)

connectTo hostname (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
	(socket AF_INET Stream proto)
	(sClose)  -- only done if there's an error
        (\sock -> do
      	  he <- getHostByName hostname
      	  connect sock (SockAddrInet port (hostAddress he))
      	  socketToHandle sock ReadWriteMode
	)

#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS) && !defined(_WIN32)
connectTo _ (UnixSocket path) = do
    bracketOnError
	(socket AF_UNIX Stream 0)
	(sClose)
	(\sock -> do
          connect sock (SockAddrUnix path)
          socketToHandle sock ReadWriteMode
	)
#endif

-- | Creates the server side socket which has been bound to the
-- specified port.  
--
-- NOTE: To avoid the \"Address already in use\"
-- problems popped up several times on the GHC-Users mailing list we
-- set the 'ReuseAddr' socket option on the listening socket.  If you
-- don't want this behaviour, please use the lower level
-- 'Network.Socket.listen' instead.

listenOn :: PortID 	-- ^ Port Identifier
	 -> IO Socket	-- ^ Connected Socket

listenOn (Service serv) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
	(sClose)
	(\sock -> do
	    port    <- getServicePortNumber serv
	    setSocketOption sock ReuseAddr 1
	    bindSocket sock (SockAddrInet port iNADDR_ANY)
	    listen sock maxListenQueue
	    return sock
	)

listenOn (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
    	(socket AF_INET Stream proto)
	(sClose)
	(\sock -> do
	    setSocketOption sock ReuseAddr 1
	    bindSocket sock (SockAddrInet port iNADDR_ANY)
	    listen sock maxListenQueue
	    return sock
	)

#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS) && !defined(_WIN32)
listenOn (UnixSocket path) =
    bracketOnError
    	(socket AF_UNIX Stream 0)
	(sClose)
	(\sock -> do
	    setSocketOption sock ReuseAddr 1
	    bindSocket sock (SockAddrUnix path)
	    listen sock maxListenQueue
	    return sock
	)
#endif

-- -----------------------------------------------------------------------------
-- accept

-- | Accept a connection on a socket created by 'listenOn'.  Normal
-- I\/O opertaions (see "System.IO") can be used on the 'Handle'
-- returned to communicate with the client.
-- Notice that although you can pass any Socket to Network.accept, only
-- sockets of either AF_UNIX or AF_INET will work (this shouldn't be a problem,
-- though). When using AF_UNIX, HostName will be set to the path of the socket
-- and PortNumber to -1.
--
accept :: Socket 		-- ^ Listening Socket
       -> IO (Handle,
	      HostName,
	      PortNumber)	-- ^ Triple of: read\/write 'Handle' for 
				-- communicating with the client,
			 	-- the 'HostName' of the peer socket, and
				-- the 'PortNumber' of the remote connection.
accept sock@(MkSocket _ AF_INET _ _ _) = do
 ~(sock', (SockAddrInet port haddr)) <- Socket.accept sock
 peer <- Exception.catchJust ioErrors
	  (do 	
	     (HostEntry peer _ _ _) <- getHostByAddr AF_INET haddr
	     return peer
	  )
	  (\e -> inet_ntoa haddr)
		-- if getHostByName fails, we fall back to the IP address
 handle <- socketToHandle sock' ReadWriteMode
 return (handle, peer, port)
#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS) && !defined(_WIN32)
accept sock@(MkSocket _ AF_UNIX _ _ _) = do
 ~(sock', (SockAddrUnix path)) <- Socket.accept sock
 handle <- socketToHandle sock' ReadWriteMode
 return (handle, path, -1)
#endif
accept sock@(MkSocket _ family _ _ _) =
  error $ "Sorry, address family " ++ (show family) ++ " is not supported!"

-- -----------------------------------------------------------------------------
-- sendTo/recvFrom

-- Send and recived data from/to the given host and port number.  These
-- should normally only be used where the socket will not be required for
-- further calls.

sendTo :: HostName 	-- Hostname
       -> PortID	-- Port Number
       -> String	-- Message to send
       -> IO ()
sendTo h p msg = do
  s <- connectTo h p
  hPutStr s msg
  hClose s

recvFrom :: HostName 	-- Hostname
	 -> PortID	-- Port Number
	 -> IO String	-- Received Data
recvFrom host port = do
 ip  <- getHostByName host
 let ipHs = hostAddresses ip
 s   <- listenOn port
 let 
  waiting = do
     ~(s', SockAddrInet _ haddr)  <-  Socket.accept s
     he <- getHostByAddr AF_INET haddr
     if not (any (`elem` ipHs) (hostAddresses he))
      then do
         sClose s'
         waiting
      else do
	h <- socketToHandle s' ReadMode
        msg <- hGetContents h
        return msg

 message <- waiting
 return message

-- ---------------------------------------------------------------------------
-- Access function returning the port type/id of socket.

-- | Returns the 'PortID' associated with a given socket.
socketPort :: Socket -> IO PortID
socketPort s = do
    sockaddr <- getSocketName s
    return (portID sockaddr)
  where
   portID sa =
    case sa of
     SockAddrInet port _    -> PortNumber port
#if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS) && !defined(_WIN32)
     SockAddrUnix path	    -> UnixSocket path
#endif

-- ---------------------------------------------------------------------------
-- Utils

-- Like bracket, but only performs the final action if there was an 
-- exception raised by the middle bit.
bracketOnError
	:: IO a		-- ^ computation to run first (\"acquire resource\")
	-> (a -> IO b)  -- ^ computation to run last (\"release resource\")
	-> (a -> IO c)	-- ^ computation to run in-between
	-> IO c		-- returns the value from the in-between computation
bracketOnError before after thing =
  block (do
    a <- before 
    r <- Exception.catch 
	   (unblock (thing a))
	   (\e -> do { after a; throw e })
    return r
 )

-----------------------------------------------------------------------------
-- Extra documentation

{-$buffering

The 'Handle' returned by 'connectTo' and 'accept' is block-buffered by
default.  For an interactive application you may want to set the
buffering mode on the 'Handle' to
'LineBuffering' or 'NoBuffering', like so:

> h <- connectTo host port
> hSetBuffering h LineBuffering
-}

{-$performance

For really fast I\/O, it might be worth looking at the 'hGetBuf' and
'hPutBuf' family of functions in "System.IO".
-}

{-$sigpipe

On Unix, when reading from a socket and the writing end is
closed by the remote client, the program is normally sent a
@SIGPIPE@ signal by the operating system.  The
default behaviour when a @SIGPIPE@ is received is
to terminate the program silently, which can be somewhat confusing
if you haven't encountered this before.  The solution is to
specify that @SIGPIPE@ is to be ignored, using
the POSIX library:

>  import Posix
>  main = do installHandler sigPIPE Ignore Nothing; ...
-}
