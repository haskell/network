{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).
--
-- A higher level interface to networking operations is provided
-- through the module "Network".
--
-----------------------------------------------------------------------------

#include "HsNet.h"

#if defined(HAVE_WINSOCK_H) && !defined(cygwin32_TARGET_OS)
#define WITH_WINSOCK  1
#endif

#if !defined(mingw32_TARGET_OS) && !defined(_WIN32)
#define DOMAIN_SOCKET_SUPPORT 1
#endif

#if !defined(CALLCONV)
#ifdef WITH_WINSOCK
#define CALLCONV stdcall
#else
#define CALLCONV ccall
#endif
#endif

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket (

    -- * Types
    Socket(..),		-- instance Eq, Show
    Family(..),		
    SocketType(..),
    SockAddr(..),
    SocketStatus(..),
    HostAddress,
    ShutdownCmd(..),
    ProtocolNumber,
    PortNumber(..),

    -- * Socket Operations
    socket,		-- :: Family -> SocketType -> ProtocolNumber -> IO Socket 
#if defined(DOMAIN_SOCKET_SUPPORT)
    socketPair,         -- :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)
#endif
    connect,		-- :: Socket -> SockAddr -> IO ()
    bindSocket,		-- :: Socket -> SockAddr -> IO ()
    listen,		-- :: Socket -> Int -> IO ()
    accept,		-- :: Socket -> IO (Socket, SockAddr)
    getPeerName,	-- :: Socket -> IO SockAddr
    getSocketName,	-- :: Socket -> IO SockAddr

#ifdef SO_PEERCRED
	-- get the credentials of our domain socket peer.
    getPeerCred,         -- :: Socket -> IO (CUInt{-pid-}, CUInt{-uid-}, CUInt{-gid-})
#endif

    socketPort,		-- :: Socket -> IO PortNumber

    socketToHandle,	-- :: Socket -> IOMode -> IO Handle

    sendTo,		-- :: Socket -> String -> SockAddr -> IO Int
    recvFrom,		-- :: Socket -> Int -> IO (String, Int, SockAddr)
    
    send,		-- :: Socket -> String -> IO Int
    recv,		-- :: Socket -> Int    -> IO String
    recvLen,            -- :: Socket -> Int    -> IO (String, Int)

    inet_addr,		-- :: String -> IO HostAddress
    inet_ntoa,		-- :: HostAddress -> IO String

    shutdown,		-- :: Socket -> ShutdownCmd -> IO ()
    sClose,		-- :: Socket -> IO ()

    -- ** Predicates on sockets
    sIsConnected,	-- :: Socket -> IO Bool
    sIsBound,		-- :: Socket -> IO Bool
    sIsListening,	-- :: Socket -> IO Bool 
    sIsReadable,	-- :: Socket -> IO Bool
    sIsWritable,	-- :: Socket -> IO Bool

    -- * Socket options
    SocketOption(..),
    getSocketOption,     -- :: Socket -> SocketOption -> IO Int
    setSocketOption,     -- :: Socket -> SocketOption -> Int -> IO ()

    -- * File descriptor transmission
#ifdef DOMAIN_SOCKET_SUPPORT
    sendFd,              -- :: Socket -> CInt -> IO ()
    recvFd,              -- :: Socket -> IO CInt

      -- Note: these two will disappear shortly
    sendAncillary,       -- :: Socket -> Int -> Int -> Int -> Ptr a -> Int -> IO ()
    recvAncillary,       -- :: Socket -> Int -> Int -> IO (Int,Int,Int,Ptr a)

#endif

    -- * Special Constants
    aNY_PORT,		-- :: PortNumber
    iNADDR_ANY,		-- :: HostAddress
    sOMAXCONN,		-- :: Int
    sOL_SOCKET,         -- :: Int
#ifdef SCM_RIGHTS
    sCM_RIGHTS,         -- :: Int
#endif
    maxListenQueue,	-- :: Int

    -- * Initialisation
    withSocketsDo,	-- :: IO a -> IO a
    
    -- * Very low level operations
     -- in case you ever want to get at the underlying file descriptor..
    fdSocket,           -- :: Socket -> CInt
    mkSocket,           -- :: CInt   -> Family 
    			-- -> SocketType
			-- -> ProtocolNumber
			-- -> SocketStatus
			-- -> IO Socket

    -- * Internal

    -- | The following are exported ONLY for use in the BSD module and
    -- should not be used anywhere else.

    packFamily, unpackFamily,
    packSocketType,
    throwSocketErrorIfMinus1_

) where

#ifdef __HUGS__
import Hugs.Prelude
import Hugs.IO ( openFd )

{-# CBITS HsNet.c initWinSock.c ancilData.c winSockErr.c #-}
#endif

import Data.Word ( Word8, Word16, Word32 )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Foreign.C.Error
import Foreign.C.String ( withCString, peekCString, peekCStringLen, castCharToCChar )
import Foreign.C.Types ( CInt, CUInt, CChar, CSize )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray0 )
import Foreign.Marshal.Utils ( with )

import System.IO
import Control.Monad ( liftM, when )
import Data.Ratio ( (%) )

import qualified Control.Exception
import Control.Concurrent.MVar

#ifdef __GLASGOW_HASKELL__
import GHC.Conc		(threadWaitRead, threadWaitWrite)
# if defined(mingw32_TARGET_OS)
import GHC.Conc         (asyncDoProc)
import Foreign( FunPtr )
# endif
import GHC.Handle
import GHC.IOBase
import qualified System.Posix.Internals
#endif

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

data SocketStatus
  -- Returned Status	Function called
  = NotConnected	-- socket
  | Bound		-- bindSocket
  | Listening		-- listen
  | Connected		-- connect/accept
    deriving (Eq, Show)

data Socket
  = MkSocket
	    CInt	         -- File Descriptor
	    Family				  
	    SocketType				  
	    ProtocolNumber	 -- Protocol Number
	    (MVar SocketStatus)  -- Status Flag

mkSocket :: CInt
	 -> Family
	 -> SocketType
	 -> ProtocolNumber
	 -> SocketStatus
	 -> IO Socket
mkSocket fd fam sType pNum stat = do
   mStat <- newMVar stat
   return (MkSocket fd fam sType pNum mStat)

instance Eq Socket where
  (MkSocket _ _ _ _ m1) == (MkSocket _ _ _ _ m2) = m1 == m2

instance Show Socket where
  showsPrec n (MkSocket fd _ _ _ _) = 
	showString "<socket: " . shows fd . showString ">"


fdSocket :: Socket -> CInt
fdSocket (MkSocket fd _ _ _ _) = fd

type ProtocolNumber = CInt

-- NOTE: HostAddresses are represented in network byte order.
--       Functions that expect the address in machine byte order
--       will have to perform the necessary translation.
type HostAddress = Word32

----------------------------------------------------------------------------
-- Port Numbers
--
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.
--
newtype PortNumber = PortNum Word16 deriving ( Eq, Ord )

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
--foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
		  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `liftM` peek (castPtr p)

-----------------------------------------------------------------------------
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
-- families. Currently only Unix domain sockets and the Internet family
-- are supported.

data SockAddr		-- C Names				
  = SockAddrInet
	PortNumber	-- sin_port  (network byte order)
	HostAddress	-- sin_addr  (ditto)
#if defined(DOMAIN_SOCKET_SUPPORT)
  | SockAddrUnix
        String          -- sun_path
#endif
  deriving (Eq)

#if defined(WITH_WINSOCK) || defined(cygwin32_TARGET_OS)
type CSaFamily = (#type unsigned short)
#elif defined(darwin_TARGET_OS)
type CSaFamily = (#type u_char)
#else
type CSaFamily = (#type sa_family_t)
#endif

-- we can't write an instance of Storable for SockAddr, because the Storable
-- class can't easily handle alternatives.

#if defined(DOMAIN_SOCKET_SUPPORT)
pokeSockAddr p (SockAddrUnix path) = do
	(#poke struct sockaddr_un, sun_family) p ((#const AF_UNIX) :: CSaFamily)
	let pathC = map castCharToCChar path
	pokeArray0 0 ((#ptr struct sockaddr_un, sun_path) p) pathC
#endif
pokeSockAddr p (SockAddrInet (PortNum port) addr) = do
	(#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
	(#poke struct sockaddr_in, sin_port) p port
	(#poke struct sockaddr_in, sin_addr) p addr	

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

-- size of struct sockaddr by family
#if defined(DOMAIN_SOCKET_SUPPORT)
sizeOfSockAddr_Family AF_UNIX = #const sizeof(struct sockaddr_un)
#endif
sizeOfSockAddr_Family AF_INET = #const sizeof(struct sockaddr_in)

-- size of struct sockaddr by SockAddr
#if defined(DOMAIN_SOCKET_SUPPORT)
sizeOfSockAddr (SockAddrUnix _)   = #const sizeof(struct sockaddr_un)
#endif
sizeOfSockAddr (SockAddrInet _ _) = #const sizeof(struct sockaddr_in)

withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
 let sz = sizeOfSockAddr addr
 allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

withNewSockAddr :: Family -> (Ptr SockAddr -> Int -> IO a) -> IO a
withNewSockAddr family f = do
 let sz = sizeOfSockAddr_Family family
 allocaBytes sz $ \ptr -> f ptr sz

-----------------------------------------------------------------------------
-- Connection Functions

-- In the following connection and binding primitives.  The names of
-- the equivalent C functions have been preserved where possible. It
-- should be noted that some of these names used in the C library,
-- \tr{bind} in particular, have a different meaning to many Haskell
-- programmers and have thus been renamed by appending the prefix
-- Socket.

-- Create an unconnected socket of the given family, type and
-- protocol.  The most common invocation of $socket$ is the following:
--    ...
--    my_socket <- socket AF_INET Stream 6
--    ...

socket :: Family 	 -- Family Name (usually AF_INET)
       -> SocketType 	 -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket	 -- Unconnected Socket

socket family stype protocol = do
    fd <- throwSocketErrorIfMinus1Retry "socket" $
		c_socket (packFamily family) (packSocketType stype) protocol
#if !defined(__HUGS__)
    System.Posix.Internals.setNonBlockingFD fd
#endif
    socket_status <- newMVar NotConnected
    return (MkSocket fd family stype protocol socket_status)

-- Create an unnamed pair of connected sockets, given family, type and
-- protocol. Differs from a normal pipe in being a bi-directional channel
-- of communication.

#if defined(DOMAIN_SOCKET_SUPPORT)
socketPair :: Family 	          -- Family Name (usually AF_INET)
           -> SocketType 	  -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
socketPair family stype protocol = do
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
    rc <- throwSocketErrorIfMinus1Retry "socketpair" $
		c_socketpair (packFamily family)
			     (packSocketType stype)
			     protocol fdArr
    [fd1,fd2] <- peekArray 2 fdArr 
    s1 <- mkSocket fd1
    s2 <- mkSocket fd2
    return (s1,s2)
  where
    mkSocket fd = do
#if !defined(__HUGS__)
       System.Posix.Internals.setNonBlockingFD fd
#endif
       stat <- newMVar Connected
       return (MkSocket fd family stype protocol stat)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
#endif

-----------------------------------------------------------------------------
-- Binding a socket
--
-- Given a port number this {\em binds} the socket to that port. This
-- means that the programmer is only interested in data being sent to
-- that port number. The $Family$ passed to $bindSocket$ must
-- be the same as that passed to $socket$.	 If the special port
-- number $aNY\_PORT$ is passed then the system assigns the next
-- available use port.
-- 
-- Port numbers for standard unix services can be found by calling
-- $getServiceEntry$.  These are traditionally port numbers below
-- 1000; although there are afew, namely NFS and IRC, which used higher
-- numbered ports.
-- 
-- The port number allocated to a socket bound by using $aNY\_PORT$ can be
-- found by calling $port$

bindSocket :: Socket	-- Unconnected Socket
	   -> SockAddr	-- Address to Bind to
	   -> IO ()

bindSocket (MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \ status -> do
 if status /= NotConnected 
  then
   ioError (userError ("bindSocket: can't peform bind on socket in status " ++
	 show status))
  else do
   withSockAddr addr $ \p_addr sz -> do
   status <- throwSocketErrorIfMinus1Retry "bind" $ c_bind s p_addr (fromIntegral sz)
   return Bound

-----------------------------------------------------------------------------
-- Connecting a socket
--
-- Make a connection to an already opened socket on a given machine
-- and port.  assumes that we have already called createSocket,
-- otherwise it will fail.
--
-- This is the dual to $bindSocket$.  The {\em server} process will
-- usually bind to a port number, the {\em client} will then connect
-- to the same port number.  Port numbers of user applications are
-- normally agreed in advance, otherwise we must rely on some meta
-- protocol for telling the other side what port number we have been
-- allocated.

connect :: Socket	-- Unconnected Socket
	-> SockAddr 	-- Socket address stuff
	-> IO ()

connect sock@(MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \currentStatus -> do
 if currentStatus /= NotConnected 
  then
   ioError (userError ("connect: can't peform connect on socket in status " ++
         show currentStatus))
  else do
   withSockAddr addr $ \p_addr sz -> do

   let  connectLoop = do
       	   r <- c_connect s p_addr (fromIntegral sz)
       	   if r == -1
       	       then do 
#if !(defined(HAVE_WINSOCK_H) && !defined(cygwin32_TARGET_OS))
	       	       err <- getErrno
		       case () of
			 _ | err == eINTR       -> connectLoop
			 _ | err == eINPROGRESS -> connectBlocked
--			 _ | err == eAGAIN      -> connectBlocked
			 otherwise              -> throwErrno "connect"
#else
		       rc <- c_getLastError
		       case rc of
		         10093 -> do -- WSANOTINITIALISED
			   withSocketsDo (return ())
	       	           r <- c_connect s p_addr (fromIntegral sz)
	       	           if r == -1
			    then (c_getLastError >>= throwSocketError "connect")
			    else return r
			 _ -> throwSocketError "connect" rc
#endif
       	       else return r

	connectBlocked = do 
#if !defined(__HUGS__)
	   threadWaitWrite (fromIntegral s)
#endif
	   err <- getSocketOption sock SoError
	   if (err == 0)
	   	then return 0
	   	else do ioError (errnoToIOError "connect" 
	   			(Errno (fromIntegral err))
	   			Nothing Nothing)

   connectLoop
   return Connected

-----------------------------------------------------------------------------
-- Listen
--
-- The programmer must call $listen$ to tell the system software that
-- they are now interested in receiving data on this port.  This must
-- be called on the bound socket before any calls to read or write
-- data are made.

-- The programmer also gives a number which indicates the length of
-- the incoming queue of unread messages for this socket. On most
-- systems the maximum queue length is around 5.  To remove a message
-- from the queue for processing a call to $accept$ should be made.

listen :: Socket  -- Connected & Bound Socket
       -> Int 	  -- Queue Length
       -> IO ()

listen (MkSocket s _family _stype _protocol socketStatus) backlog = do
 modifyMVar_ socketStatus $ \ status -> do
 if status /= Bound 
   then
    ioError (userError ("listen: can't peform listen on socket in status " ++
          show status))
   else do
    throwSocketErrorIfMinus1Retry "listen" (c_listen s (fromIntegral backlog))
    return Listening

-----------------------------------------------------------------------------
-- Accept
--
-- A call to `accept' only returns when data is available on the given
-- socket, unless the socket has been set to non-blocking.  It will
-- return a new socket which should be used to read the incoming data and
-- should then be closed. Using the socket returned by `accept' allows
-- incoming requests to be queued on the original socket.

accept :: Socket			-- Queue Socket
       -> IO (Socket,			-- Readable Socket
	      SockAddr)			-- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- sIsAcceptable sock
 if not okay
   then
     ioError (userError ("accept: can't perform accept on socket (" ++ (show (family,stype,protocol)) ++") in status " ++
	 show currentStatus))
   else do
     let sz = sizeOfSockAddr_Family family
     allocaBytes sz $ \ sockaddr -> do
#if defined(mingw32_TARGET_OS) && !defined(__HUGS__)
     paramData <- c_newAcceptParams s (fromIntegral sz) sockaddr
     rc        <- asyncDoProc c_acceptDoProc paramData
     new_sock  <- c_acceptNewSock    paramData
     c_free paramData
     when (rc /= 0)
          (ioError (errnoToIOError "Network.Socket.accept" (Errno (fromIntegral rc)) Nothing Nothing))
#else 
     with (fromIntegral sz) $ \ ptr_len -> do
     new_sock <- 
# if !defined(__HUGS__)
                 throwErrnoIfMinus1Retry_repeatOnBlock "accept" 
			(threadWaitRead (fromIntegral s))
# endif
			(c_accept s sockaddr ptr_len)
# if !defined(__HUGS__)
     System.Posix.Internals.setNonBlockingFD new_sock
# endif
#endif
     addr <- peekSockAddr sockaddr
     new_status <- newMVar Connected
     return ((MkSocket new_sock family stype protocol new_status), addr)

#if defined(mingw32_TARGET_OS) && !defined(__HUGS__)
foreign import ccall unsafe "HsNet.h acceptNewSock"
  c_acceptNewSock :: Ptr () -> IO CInt
foreign import ccall unsafe "HsNet.h newAcceptParams"
  c_newAcceptParams :: CInt -> CInt -> Ptr a -> IO (Ptr ())
foreign import ccall unsafe "HsNet.h &acceptDoProc"
  c_acceptDoProc :: FunPtr (Ptr () -> IO Int)
foreign import ccall unsafe "free"
  c_free:: Ptr a -> IO ()
#endif

-----------------------------------------------------------------------------
-- sendTo & recvFrom

sendTo :: Socket	-- (possibly) bound/connected Socket
       -> String	-- Data to send
       -> SockAddr
       -> IO Int	-- Number of Bytes sent

sendTo (MkSocket s _family _stype _protocol status) xs addr = do
 withSockAddr addr $ \p_addr sz -> do
 withCString xs $ \str -> do
   liftM fromIntegral $
#if !defined(__HUGS__)
     throwErrnoIfMinus1Retry_repeatOnBlock "sendTo"
	(threadWaitWrite (fromIntegral s)) $
#endif
	c_sendto s str (fromIntegral $ length xs) 0{-flags-} 
			p_addr (fromIntegral sz)

recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock@(MkSocket s _family _stype _protocol status) nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvFrom")
 | otherwise   = 
  allocaBytes nbytes $ \ptr -> do
    withNewSockAddr AF_INET $ \ptr_addr sz -> do
      alloca $ \ptr_len -> do
      	poke ptr_len (fromIntegral sz)
        len <- 
#if !defined(__HUGS__)
	       throwErrnoIfMinus1Retry_repeatOnBlock "recvFrom" 
        	   (threadWaitRead (fromIntegral s)) $
#endif
        	   c_recvfrom s ptr (fromIntegral nbytes) 0{-flags-} 
				ptr_addr ptr_len
        let len' = fromIntegral len
	if len' == 0
	 then ioError (mkEOFError "Network.Socket.recvFrom")
	 else do
   	   flg <- sIsConnected sock
	     -- For at least one implementation (WinSock 2), recvfrom() ignores
	     -- filling in the sockaddr for connected TCP sockets. Cope with 
	     -- this by using getPeerName instead.
	   sockaddr <- 
		if flg then
		   getPeerName sock
		else
		   peekSockAddr ptr_addr 
           str <- peekCStringLen (ptr,len')
           return (str, len', sockaddr)

-----------------------------------------------------------------------------
-- send & recv

send :: Socket	-- Bound/Connected Socket
     -> String	-- Data to send
     -> IO Int	-- Number of Bytes sent
send (MkSocket s _family _stype _protocol status) xs = do
 withCString xs $ \str -> do
   liftM fromIntegral $
#if !defined(__HUGS__)
     throwErrnoIfMinus1Retry_repeatOnBlock "send"
	(threadWaitWrite (fromIntegral s)) $
#endif
	c_send s str (fromIntegral $ length xs) 0{-flags-} 

recv :: Socket -> Int -> IO String
recv sock l = recvLen sock l >>= \ (s,_) -> return s

recvLen :: Socket -> Int -> IO (String, Int)
recvLen sock@(MkSocket s _family _stype _protocol status) nbytes 
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recv")
 | otherwise   = do
     allocaBytes nbytes $ \ptr -> do
        len <- 
#if !defined(__HUGS__)
	       throwErrnoIfMinus1Retry_repeatOnBlock "recv" 
        	   (threadWaitRead (fromIntegral s)) $
#endif
        	   c_recv s ptr (fromIntegral nbytes) 0{-flags-} 
        let len' = fromIntegral len
	if len' == 0
	 then ioError (mkEOFError "Network.Socket.recv")
	 else do
	   s <- peekCStringLen (ptr,len')
	   return (s, len')

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

socketPort :: Socket		-- Connected & Bound Socket
	   -> IO PortNumber	-- Port Number of Socket
socketPort sock@(MkSocket _ AF_INET _ _ _) = do
    (SockAddrInet port _) <- getSocketName sock
    return port
socketPort (MkSocket _ family _ _ _) =
    ioError (userError ("socketPort: not supported for Family " ++ show family))


-- ---------------------------------------------------------------------------
-- getPeerName

-- Calling $getPeerName$ returns the address details of the machine,
-- other than the local one, which is connected to the socket. This is
-- used in programs such as FTP to determine where to send the
-- returning data.  The corresponding call to get the details of the
-- local machine is $getSocketName$.

getPeerName   :: Socket -> IO SockAddr
getPeerName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
   throwSocketErrorIfMinus1Retry "getPeerName" $ c_getpeername s ptr int_star
   sz <- peek int_star
   peekSockAddr ptr
    
getSocketName :: Socket -> IO SockAddr
getSocketName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
   throwSocketErrorIfMinus1Retry "getSocketName" $ c_getsockname s ptr int_star
   peekSockAddr ptr

-----------------------------------------------------------------------------
-- Socket Properties

data SocketOption
    = DummySocketOption__
#ifdef SO_DEBUG
    | Debug         {- SO_DEBUG     -}
#endif
#ifdef SO_REUSEADDR
    | ReuseAddr     {- SO_REUSEADDR -}
#endif
#ifdef SO_TYPE
    | Type          {- SO_TYPE      -}
#endif
#ifdef SO_ERROR
    | SoError       {- SO_ERROR     -}
#endif
#ifdef SO_DONTROUTE
    | DontRoute     {- SO_DONTROUTE -}
#endif
#ifdef SO_BROADCAST
    | Broadcast     {- SO_BROADCAST -}
#endif
#ifdef SO_SNDBUF
    | SendBuffer    {- SO_SNDBUF    -}
#endif
#ifdef SO_RCVBUF
    | RecvBuffer    {- SO_RCVBUF    -}
#endif
#ifdef SO_KEEPALIVE
    | KeepAlive     {- SO_KEEPALIVE -}
#endif
#ifdef SO_OOBINLINE
    | OOBInline     {- SO_OOBINLINE -}
#endif
#ifdef IP_TTL
    | TimeToLive    {- IP_TTL       -}
#endif
#ifdef TCP_MAXSEG
    | MaxSegment    {- TCP_MAXSEG   -}
#endif
#ifdef TCP_NODELAY
    | NoDelay       {- TCP_NODELAY  -}
#endif
#ifdef SO_LINGER
    | Linger        {- SO_LINGER    -}
#endif
#ifdef SO_REUSEPORT
    | ReusePort     {- SO_REUSEPORT -}
#endif
#ifdef SO_RCVLOWAT
    | RecvLowWater  {- SO_RCVLOWAT  -}
#endif
#ifdef SO_SNDLOWAT
    | SendLowWater  {- SO_SNDLOWAT  -}
#endif
#ifdef SO_RCVTIMEO
    | RecvTimeOut   {- SO_RCVTIMEO  -}
#endif
#ifdef SO_SNDTIMEO
    | SendTimeOut   {- SO_SNDTIMEO  -}
#endif
#ifdef SO_USELOOPBACK
    | UseLoopBack   {- SO_USELOOPBACK -}
#endif

socketOptLevel :: SocketOption -> CInt
socketOptLevel so = 
  case so of
#ifdef IP_TTL
    TimeToLive   -> #const IPPROTO_IP
#endif
#ifdef TCP_MAXSEG
    MaxSegment   -> #const IPPROTO_TCP
#endif
#ifdef TCP_NODELAY
    NoDelay      -> #const IPPROTO_TCP
#endif
    _            -> #const SOL_SOCKET

packSocketOption :: SocketOption -> CInt
packSocketOption so =
  case so of
#ifdef SO_DEBUG
    Debug         -> #const SO_DEBUG
#endif
#ifdef SO_REUSEADDR
    ReuseAddr     -> #const SO_REUSEADDR
#endif
#ifdef SO_TYPE
    Type          -> #const SO_TYPE
#endif
#ifdef SO_ERROR
    SoError       -> #const SO_ERROR
#endif
#ifdef SO_DONTROUTE
    DontRoute     -> #const SO_DONTROUTE
#endif
#ifdef SO_BROADCAST
    Broadcast     -> #const SO_BROADCAST
#endif
#ifdef SO_SNDBUF
    SendBuffer    -> #const SO_SNDBUF
#endif
#ifdef SO_RCVBUF
    RecvBuffer    -> #const SO_RCVBUF
#endif
#ifdef SO_KEEPALIVE
    KeepAlive     -> #const SO_KEEPALIVE
#endif
#ifdef SO_OOBINLINE
    OOBInline     -> #const SO_OOBINLINE
#endif
#ifdef IP_TTL
    TimeToLive    -> #const IP_TTL
#endif
#ifdef TCP_MAXSEG
    MaxSegment    -> #const TCP_MAXSEG
#endif
#ifdef TCP_NODELAY
    NoDelay       -> #const TCP_NODELAY
#endif
#ifdef SO_LINGER
    Linger	  -> #const SO_LINGER
#endif
#ifdef SO_REUSEPORT
    ReusePort     -> #const SO_REUSEPORT
#endif
#ifdef SO_RCVLOWAT
    RecvLowWater  -> #const SO_RCVLOWAT
#endif
#ifdef SO_SNDLOWAT
    SendLowWater  -> #const SO_SNDLOWAT
#endif
#ifdef SO_RCVTIMEO
    RecvTimeOut   -> #const SO_RCVTIMEO
#endif
#ifdef SO_SNDTIMEO
    SendTimeOut   -> #const SO_SNDTIMEO
#endif
#ifdef SO_USELOOPBACK
    UseLoopBack   -> #const SO_USELOOPBACK
#endif

setSocketOption :: Socket 
		-> SocketOption -- Option Name
		-> Int		-- Option Value
		-> IO ()
setSocketOption (MkSocket s _ _ _ _) so v = do
   with (fromIntegral v) $ \ptr_v -> do
   throwErrnoIfMinus1_ "setSocketOption" $
       c_setsockopt s (socketOptLevel so) (packSocketOption so) ptr_v 
	  (fromIntegral (sizeOf v))
   return ()


getSocketOption :: Socket
		-> SocketOption  -- Option Name
		-> IO Int	 -- Option Value
getSocketOption (MkSocket s _ _ _ _) so = do
   alloca $ \ptr_v ->
     with (fromIntegral (sizeOf (undefined :: CInt))) $ \ptr_sz -> do
       throwErrnoIfMinus1 "getSocketOption" $
	 c_getsockopt s (socketOptLevel so) (packSocketOption so) ptr_v ptr_sz
       fromIntegral `liftM` peek ptr_v


#ifdef SO_PEERCRED
-- | Returns the processID, userID and groupID of the socket's peer.
--
-- Only available on platforms that support SO_PEERCRED on domain sockets.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred sock = do
  let fd = fdSocket sock
  let sz = (fromIntegral (#const sizeof(struct ucred)))
  with sz $ \ ptr_cr -> 
   alloca       $ \ ptr_sz -> do
     poke ptr_sz sz
     throwErrnoIfMinus1 "getPeerCred" $
       c_getsockopt fd (#const SOL_SOCKET) (#const SO_PEERCRED) ptr_cr ptr_sz
     pid <- (#peek struct ucred, pid) ptr_cr
     uid <- (#peek struct ucred, uid) ptr_cr
     gid <- (#peek struct ucred, gid) ptr_cr
     return (pid, uid, gid)
#endif

#if defined(DOMAIN_SOCKET_SUPPORT)
-- sending/receiving ancillary socket data; low-level mechanism
-- for transmitting file descriptors, mainly.
sendFd :: Socket -> CInt -> IO ()
sendFd sock outfd = do
  let fd = fdSocket sock
#if !defined(__HUGS__)
  throwErrnoIfMinus1Retry_repeatOnBlock "sendFd"
     (threadWaitWrite (fromIntegral fd)) $
     c_sendFd fd outfd
#else
  c_sendFd fd outfd
#endif
   -- Note: If Winsock supported FD-passing, thi would have been 
   -- incorrect (since socket FDs need to be closed via closesocket().)
  c_close outfd
  return ()
  
recvFd :: Socket -> IO CInt
recvFd sock = do
  let fd = fdSocket sock
  theFd <- 
#if !defined(__HUGS__)
    throwErrnoIfMinus1Retry_repeatOnBlock "recvFd" 
        (threadWaitRead (fromIntegral fd)) $
#endif
         c_recvFd fd
  return theFd


sendAncillary :: Socket
	      -> Int
	      -> Int
	      -> Int
	      -> Ptr a
	      -> Int
	      -> IO ()
sendAncillary sock level ty flags datum len = do
  let fd = fdSocket sock
  _ <-
#if !defined(__HUGS__)
   throwErrnoIfMinus1Retry_repeatOnBlock "sendAncillary"
     (threadWaitWrite (fromIntegral fd)) $
#endif
     c_sendAncillary fd (fromIntegral level) (fromIntegral ty)
     			(fromIntegral flags) datum (fromIntegral len)
  return ()

recvAncillary :: Socket
	      -> Int
	      -> Int
	      -> IO (Int,Int,Ptr a,Int)
recvAncillary sock flags len = do
  let fd = fdSocket sock
  alloca      $ \ ptr_len   ->
   alloca      $ \ ptr_lev   ->
    alloca      $ \ ptr_ty    ->
     alloca      $ \ ptr_pData -> do
      poke ptr_len (fromIntegral len)
      _ <- 
#if !defined(__HUGS__)
        throwErrnoIfMinus1Retry_repeatOnBlock "recvAncillary" 
            (threadWaitRead (fromIntegral fd)) $
#endif
	    c_recvAncillary fd ptr_lev ptr_ty (fromIntegral flags) ptr_pData ptr_len
      len <- fromIntegral `liftM` peek ptr_len
      lev <- fromIntegral `liftM` peek ptr_lev
      ty  <- fromIntegral `liftM` peek ptr_ty
      pD  <- peek ptr_pData
      return (lev,ty,pD, len)
foreign import ccall unsafe "sendAncillary"
  c_sendAncillary :: CInt -> CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "recvAncillary"
  c_recvAncillary :: CInt -> Ptr CInt -> Ptr CInt -> CInt -> Ptr (Ptr a) -> Ptr CInt -> IO CInt

foreign import ccall unsafe "sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "recvFd" c_recvFd :: CInt -> IO CInt

#endif


{-
A calling sequence table for the main functions is shown in the table below.

\begin{figure}[h]
\begin{center}
\begin{tabular}{|l|c|c|c|c|c|c|c|}d
\hline
{\bf A Call to} & socket & connect & bindSocket & listen & accept & read & write \\
\hline
{\bf Precedes} & & & & & & & \\
\hline 
socket &	&	  &	       &	&	 &	& \\
\hline
connect & +	&	  &	       &	&	 &	& \\
\hline
bindSocket & +	&	  &	       &	&	 &	& \\
\hline
listen &	&	  & +	       &	&	 &	& \\
\hline
accept &	&	  &	       &  +	&	 &	& \\
\hline
read   &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
write  &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
\end{tabular}
\caption{Sequence Table for Major functions of Socket}
\label{tab:api-seq}
\end{center}
\end{figure}
-}

-- ---------------------------------------------------------------------------
-- OS Dependent Definitions
    
unpackFamily	:: CInt -> Family
packFamily	:: Family -> CInt

packSocketType	:: SocketType -> CInt

-- | Address Families.
--
-- This data type might have different constructors depending on what is
-- supported by the operating system.
data Family
	= AF_UNSPEC	-- unspecified
#ifdef AF_UNIX
	| AF_UNIX	-- local to host (pipes, portals
#endif
#ifdef AF_INET
	| AF_INET	-- internetwork: UDP, TCP, etc
#endif
#ifdef AF_INET6
        | AF_INET6	-- Internet Protocol version 6
#endif
#ifdef AF_IMPLINK
	| AF_IMPLINK	-- arpanet imp addresses
#endif
#ifdef AF_PUP
	| AF_PUP	-- pup protocols: e.g. BSP
#endif
#ifdef AF_CHAOS
	| AF_CHAOS	-- mit CHAOS protocols
#endif
#ifdef AF_NS
	| AF_NS		-- XEROX NS protocols 
#endif
#ifdef AF_NBS
	| AF_NBS	-- nbs protocols
#endif
#ifdef AF_ECMA
	| AF_ECMA	-- european computer manufacturers
#endif
#ifdef AF_DATAKIT
	| AF_DATAKIT	-- datakit protocols
#endif
#ifdef AF_CCITT
	| AF_CCITT	-- CCITT protocols, X.25 etc
#endif
#ifdef AF_SNA
	| AF_SNA	-- IBM SNA
#endif
#ifdef AF_DECnet
	| AF_DECnet	-- DECnet
#endif
#ifdef AF_DLI
	| AF_DLI	-- Direct data link interface
#endif
#ifdef AF_LAT
	| AF_LAT	-- LAT
#endif
#ifdef AF_HYLINK
	| AF_HYLINK	-- NSC Hyperchannel
#endif
#ifdef AF_APPLETALK
	| AF_APPLETALK	-- Apple Talk
#endif
#ifdef AF_ROUTE
	| AF_ROUTE	-- Internal Routing Protocol 
#endif
#ifdef AF_NETBIOS
	| AF_NETBIOS	-- NetBios-style addresses
#endif
#ifdef AF_NIT
	| AF_NIT	-- Network Interface Tap
#endif
#ifdef AF_802
	| AF_802	-- IEEE 802.2, also ISO 8802
#endif
#ifdef AF_ISO
	| AF_ISO	-- ISO protocols
#endif
#ifdef AF_OSI
	| AF_OSI	-- umbrella of all families used by OSI
#endif
#ifdef AF_NETMAN
	| AF_NETMAN	-- DNA Network Management 
#endif
#ifdef AF_X25
	| AF_X25	-- CCITT X.25
#endif
#ifdef AF_AX25
	| AF_AX25
#endif
#ifdef AF_OSINET
	| AF_OSINET	-- AFI
#endif
#ifdef AF_GOSSIP
	| AF_GOSSIP	-- US Government OSI
#endif
#ifdef AF_IPX
	| AF_IPX	-- Novell Internet Protocol
#endif
#ifdef Pseudo_AF_XTP
	| Pseudo_AF_XTP	-- eXpress Transfer Protocol (no AF) 
#endif
#ifdef AF_CTF
	| AF_CTF	-- Common Trace Facility 
#endif
#ifdef AF_WAN
	| AF_WAN	-- Wide Area Network protocols 
#endif
#ifdef AF_SDL
        | AF_SDL	-- SGI Data Link for DLPI
#endif
#ifdef AF_NETWARE
        | AF_NETWARE	
#endif
#ifdef AF_NDD
        | AF_NDD		
#endif
#ifdef AF_INTF
        | AF_INTF	-- Debugging use only 
#endif
#ifdef AF_COIP
        | AF_COIP         -- connection-oriented IP, aka ST II
#endif
#ifdef AF_CNT
        | AF_CNT	-- Computer Network Technology
#endif
#ifdef Pseudo_AF_RTIP
        | Pseudo_AF_RTIP  -- Help Identify RTIP packets
#endif
#ifdef Pseudo_AF_PIP
        | Pseudo_AF_PIP   -- Help Identify PIP packets
#endif
#ifdef AF_SIP
        | AF_SIP          -- Simple Internet Protocol
#endif
#ifdef AF_ISDN
        | AF_ISDN         -- Integrated Services Digital Network
#endif
#ifdef Pseudo_AF_KEY
        | Pseudo_AF_KEY   -- Internal key-management function
#endif
#ifdef AF_NATM
        | AF_NATM         -- native ATM access
#endif
#ifdef AF_ARP
        | AF_ARP          -- (rev.) addr. res. prot. (RFC 826)
#endif
#ifdef Pseudo_AF_HDRCMPLT
        | Pseudo_AF_HDRCMPLT -- Used by BPF to not rewrite hdrs in iface output
#endif
#ifdef AF_ENCAP
        | AF_ENCAP 
#endif
#ifdef AF_LINK
	| AF_LINK	-- Link layer interface 
#endif
#ifdef AF_RAW
        | AF_RAW	-- Link layer interface
#endif
#ifdef AF_RIF
        | AF_RIF	-- raw interface 
#endif
	deriving (Eq, Ord, Read, Show)

------ ------
			
packFamily f = case f of
	AF_UNSPEC -> #const AF_UNSPEC
#ifdef AF_UNIX
	AF_UNIX -> #const AF_UNIX
#endif
#ifdef AF_INET
	AF_INET -> #const AF_INET
#endif
#ifdef AF_INET6
        AF_INET6 -> #const AF_INET6
#endif
#ifdef AF_IMPLINK
	AF_IMPLINK -> #const AF_IMPLINK
#endif
#ifdef AF_PUP
	AF_PUP -> #const AF_PUP
#endif
#ifdef AF_CHAOS
	AF_CHAOS -> #const AF_CHAOS
#endif
#ifdef AF_NS
	AF_NS -> #const AF_NS
#endif
#ifdef AF_NBS
	AF_NBS -> #const AF_NBS
#endif
#ifdef AF_ECMA
	AF_ECMA -> #const AF_ECMA
#endif
#ifdef AF_DATAKIT
	AF_DATAKIT -> #const AF_DATAKIT
#endif
#ifdef AF_CCITT
	AF_CCITT -> #const AF_CCITT
#endif
#ifdef AF_SNA
	AF_SNA -> #const AF_SNA
#endif
#ifdef AF_DECnet
	AF_DECnet -> #const AF_DECnet
#endif
#ifdef AF_DLI
	AF_DLI -> #const AF_DLI
#endif
#ifdef AF_LAT
	AF_LAT -> #const AF_LAT
#endif
#ifdef AF_HYLINK
	AF_HYLINK -> #const AF_HYLINK
#endif
#ifdef AF_APPLETALK
	AF_APPLETALK -> #const AF_APPLETALK
#endif
#ifdef AF_ROUTE
	AF_ROUTE -> #const AF_ROUTE
#endif
#ifdef AF_NETBIOS
	AF_NETBIOS -> #const AF_NETBIOS
#endif
#ifdef AF_NIT
	AF_NIT -> #const AF_NIT
#endif
#ifdef AF_802
	AF_802 -> #const AF_802
#endif
#ifdef AF_ISO
	AF_ISO -> #const AF_ISO
#endif
#ifdef AF_OSI
	AF_OSI -> #const AF_OSI
#endif
#ifdef AF_NETMAN
	AF_NETMAN -> #const AF_NETMAN
#endif
#ifdef AF_X25
	AF_X25 -> #const AF_X25
#endif
#ifdef AF_AX25
	AF_AX25 -> #const AF_AX25
#endif
#ifdef AF_OSINET
	AF_OSINET -> #const AF_OSINET
#endif
#ifdef AF_GOSSIP
	AF_GOSSIP -> #const AF_GOSSIP
#endif
#ifdef AF_IPX
	AF_IPX -> #const AF_IPX
#endif
#ifdef Pseudo_AF_XTP
	Pseudo_AF_XTP -> #const Pseudo_AF_XTP
#endif
#ifdef AF_CTF
	AF_CTF -> #const AF_CTF
#endif
#ifdef AF_WAN
	AF_WAN -> #const AF_WAN
#endif
#ifdef AF_SDL
        AF_SDL -> #const AF_SDL
#endif
#ifdef AF_NETWARE
        AF_NETWARE -> #const AF_NETWARE	
#endif
#ifdef AF_NDD
        AF_NDD -> #const AF_NDD		
#endif
#ifdef AF_INTF
        AF_INTF -> #const AF_INTF
#endif
#ifdef AF_COIP
        AF_COIP -> #const AF_COIP
#endif
#ifdef AF_CNT
        AF_CNT -> #const AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
        Pseudo_AF_RTIP -> #const Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
        Pseudo_AF_PIP -> #const Pseudo_AF_PIP
#endif
#ifdef AF_SIP
        AF_SIP -> #const AF_SIP
#endif
#ifdef AF_ISDN
        AF_ISDN -> #const AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
        Pseudo_AF_KEY -> #const Pseudo_AF_KEY
#endif
#ifdef AF_NATM
        AF_NATM -> #const AF_NATM
#endif
#ifdef AF_ARP
        AF_ARP -> #const AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
        Pseudo_AF_HDRCMPLT -> #const Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
        AF_ENCAP -> #const AF_ENCAP 
#endif
#ifdef AF_LINK
	AF_LINK -> #const AF_LINK
#endif
#ifdef AF_RAW
        AF_RAW -> #const AF_RAW
#endif
#ifdef AF_RIF
        AF_RIF -> #const AF_RIF
#endif

--------- ----------

unpackFamily f = case f of
	(#const AF_UNSPEC) -> AF_UNSPEC
#ifdef AF_UNIX
	(#const AF_UNIX) -> AF_UNIX
#endif
#ifdef AF_INET
	(#const AF_INET) -> AF_INET
#endif
#ifdef AF_INET6
        (#const AF_INET6) -> AF_INET6
#endif
#ifdef AF_IMPLINK
	(#const AF_IMPLINK) -> AF_IMPLINK
#endif
#ifdef AF_PUP
	(#const AF_PUP) -> AF_PUP
#endif
#ifdef AF_CHAOS
	(#const AF_CHAOS) -> AF_CHAOS
#endif
#ifdef AF_NS
	(#const AF_NS) -> AF_NS
#endif
#ifdef AF_NBS
	(#const AF_NBS) -> AF_NBS
#endif
#ifdef AF_ECMA
	(#const AF_ECMA) -> AF_ECMA
#endif
#ifdef AF_DATAKIT
	(#const AF_DATAKIT) -> AF_DATAKIT
#endif
#ifdef AF_CCITT
	(#const AF_CCITT) -> AF_CCITT
#endif
#ifdef AF_SNA
	(#const AF_SNA) -> AF_SNA
#endif
#ifdef AF_DECnet
	(#const AF_DECnet) -> AF_DECnet
#endif
#ifdef AF_DLI
	(#const AF_DLI) -> AF_DLI
#endif
#ifdef AF_LAT
	(#const AF_LAT) -> AF_LAT
#endif
#ifdef AF_HYLINK
	(#const AF_HYLINK) -> AF_HYLINK
#endif
#ifdef AF_APPLETALK
	(#const AF_APPLETALK) -> AF_APPLETALK
#endif
#ifdef AF_ROUTE
	(#const AF_ROUTE) -> AF_ROUTE
#endif
#ifdef AF_NETBIOS
	(#const AF_NETBIOS) -> AF_NETBIOS
#endif
#ifdef AF_NIT
	(#const AF_NIT) -> AF_NIT
#endif
#ifdef AF_802
	(#const AF_802) -> AF_802
#endif
#ifdef AF_ISO
	(#const AF_ISO) -> AF_ISO
#endif
#ifdef AF_OSI
# if (!defined(AF_ISO)) || (defined(AF_ISO) && (AF_ISO != AF_OSI))
	(#const AF_OSI) -> AF_OSI
# endif
#endif
#ifdef AF_NETMAN
	(#const AF_NETMAN) -> AF_NETMAN
#endif
#ifdef AF_X25
	(#const AF_X25) -> AF_X25
#endif
#ifdef AF_AX25
	(#const AF_AX25) -> AF_AX25
#endif
#ifdef AF_OSINET
	(#const AF_OSINET) -> AF_OSINET
#endif
#ifdef AF_GOSSIP
	(#const AF_GOSSIP) -> AF_GOSSIP
#endif
#ifdef AF_IPX
	(#const AF_IPX) -> AF_IPX
#endif
#ifdef Pseudo_AF_XTP
	(#const Pseudo_AF_XTP) -> Pseudo_AF_XTP
#endif
#ifdef AF_CTF
	(#const AF_CTF) -> AF_CTF
#endif
#ifdef AF_WAN
	(#const AF_WAN) -> AF_WAN
#endif
#ifdef AF_SDL
        (#const AF_SDL) -> AF_SDL
#endif
#ifdef AF_NETWARE
        (#const AF_NETWARE) -> AF_NETWARE	
#endif
#ifdef AF_NDD
        (#const AF_NDD) -> AF_NDD		
#endif
#ifdef AF_INTF
        (#const AF_INTF) -> AF_INTF
#endif
#ifdef AF_COIP
        (#const AF_COIP) -> AF_COIP
#endif
#ifdef AF_CNT
        (#const AF_CNT) -> AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
        (#const Pseudo_AF_RTIP) -> Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
        (#const Pseudo_AF_PIP) -> Pseudo_AF_PIP
#endif
#ifdef AF_SIP
        (#const AF_SIP) -> AF_SIP
#endif
#ifdef AF_ISDN
        (#const AF_ISDN) -> AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
        (#const Pseudo_AF_KEY) -> Pseudo_AF_KEY
#endif
#ifdef AF_NATM
        (#const AF_NATM) -> AF_NATM
#endif
#ifdef AF_ARP
        (#const AF_ARP) -> AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
        (#const Pseudo_AF_HDRCMPLT) -> Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
        (#const AF_ENCAP) -> AF_ENCAP 
#endif
#ifdef AF_LINK
	(#const AF_LINK) -> AF_LINK
#endif
#ifdef AF_RAW
        (#const AF_RAW) -> AF_RAW
#endif
#ifdef AF_RIF
        (#const AF_RIF) -> AF_RIF
#endif

-- Socket Types.

-- | Socket Types.
--
-- This data type might have different constructors depending on what is
-- supported by the operating system.
data SocketType
	= NoSocketType
#ifdef SOCK_STREAM
	| Stream 
#endif
#ifdef SOCK_DGRAM
	| Datagram
#endif
#ifdef SOCK_RAW
	| Raw 
#endif
#ifdef SOCK_RDM
	| RDM 
#endif
#ifdef SOCK_SEQPACKET
	| SeqPacket
#endif
	deriving (Eq, Ord, Read, Show)
	
packSocketType stype = case stype of
	NoSocketType -> 0
#ifdef SOCK_STREAM
	Stream -> #const SOCK_STREAM
#endif
#ifdef SOCK_DGRAM
	Datagram -> #const SOCK_DGRAM
#endif
#ifdef SOCK_RAW
	Raw -> #const SOCK_RAW
#endif
#ifdef SOCK_RDM
	RDM -> #const SOCK_RDM
#endif
#ifdef SOCK_SEQPACKET
	SeqPacket -> #const SOCK_SEQPACKET
#endif

-- ---------------------------------------------------------------------------
-- Utility Functions

aNY_PORT :: PortNumber 
aNY_PORT = 0

iNADDR_ANY :: HostAddress
iNADDR_ANY = htonl (#const INADDR_ANY)

sOMAXCONN :: Int
sOMAXCONN = #const SOMAXCONN

sOL_SOCKET :: Int
sOL_SOCKET = #const SOL_SOCKET

#ifdef SCM_RIGHTS
sCM_RIGHTS :: Int
sCM_RIGHTS = #const SCM_RIGHTS
#endif

maxListenQueue :: Int
maxListenQueue = sOMAXCONN

-- -----------------------------------------------------------------------------

data ShutdownCmd 
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry "shutdown" (c_shutdown s (sdownCmdToInt stype))
  return ()

-- -----------------------------------------------------------------------------

sClose	 :: Socket -> IO ()
sClose (MkSocket s _ _ _ _) = do c_close s; return ()

-- -----------------------------------------------------------------------------

sIsConnected :: Socket -> IO Bool
sIsConnected (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected)	

-- -----------------------------------------------------------------------------
-- Socket Predicates

sIsBound :: Socket -> IO Bool
sIsBound (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Bound)	

sIsListening :: Socket -> IO Bool
sIsListening (MkSocket _ _ _  _ status) = do
    value <- readMVar status
    return (value == Listening)	

sIsReadable  :: Socket -> IO Bool
sIsReadable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Listening || value == Connected)

sIsWritable  :: Socket -> IO Bool
sIsWritable = sIsReadable -- sort of.

sIsAcceptable :: Socket -> IO Bool
#if defined(DOMAIN_SOCKET_SUPPORT)
sIsAcceptable (MkSocket _ AF_UNIX Stream _ status) = do
    value <- readMVar status
    return (value == Connected || value == Bound || value == Listening)
sIsAcceptable (MkSocket _ AF_UNIX _ _ _) = return False
#endif
sIsAcceptable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected || value == Listening)
    
-- -----------------------------------------------------------------------------
-- Internet address manipulation routines:

inet_addr :: String -> IO HostAddress
inet_addr ipstr = do
   withCString ipstr $ \str -> do
   had <- c_inet_addr str
   if had == -1
    then ioError (userError ("inet_addr: Malformed address: " ++ ipstr))
    else return had  -- network byte order

inet_ntoa :: HostAddress -> IO String
inet_ntoa haddr = do
  pstr <- c_inet_ntoa haddr
  peekCString pstr

-- socketHandle turns a Socket into a Haskell IO Handle. By default, the new
-- handle is unbuffered. Use hSetBuffering to alter this.

#ifndef __PARALLEL_HASKELL__
socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle s@(MkSocket fd _ _ _ _) mode = do
# ifdef __GLASGOW_HASKELL__
    openFd (fromIntegral fd) (Just System.Posix.Internals.Stream) (show s) mode True{-bin-} False{-no truncate-}
# endif
# ifdef __HUGS__
    openFd (fromIntegral fd) True{-is a socket-} mode True{-bin-}
# endif
#else
socketToHandle (MkSocket s family stype protocol status) m =
  error "socketToHandle not implemented in a parallel setup"
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = IOError Nothing 
#ifdef __GLASGOW_HASKELL__
				    InvalidArgument
#else
				    IllegalOperation
#endif
				    loc "non-positive length" Nothing

mkEOFError :: String -> IOError
mkEOFError loc = IOError Nothing EOF loc "end of file" Nothing

-- ---------------------------------------------------------------------------
-- WinSock support

{-| On Windows operating systems, the networking subsystem has to be
initialised using 'withSocketsDo' before any networking operations can
be used.  eg.

> main = withSocketsDo $ do {...}

Although this is only strictly necessary on Windows platforms, it is
harmless on other platforms, so for portability it is good practice to
use it all the time.
-}
withSocketsDo :: IO a -> IO a
#if !defined(WITH_WINSOCK)
withSocketsDo x = x
#else
withSocketsDo act = do
   x <- initWinSock
   if ( x /= 0 ) then
     ioError (userError "Failed to initialise WinSock")
    else do
      act `Control.Exception.finally` shutdownWinSock

foreign import ccall unsafe "initWinSock" initWinSock :: IO Int
foreign import ccall unsafe "shutdownWinSock" shutdownWinSock :: IO ()

#endif

-- ---------------------------------------------------------------------------
-- foreign imports from the C library

foreign import ccall unsafe "my_inet_ntoa"
  c_inet_ntoa :: HostAddress -> IO (Ptr CChar)

foreign import CALLCONV unsafe "inet_addr"
  c_inet_addr :: Ptr CChar -> IO HostAddress

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt 

#if !defined(WITH_WINSOCK)
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt
#else
foreign import stdcall unsafe "closesocket"
  c_close :: CInt -> IO CInt
#endif

foreign import CALLCONV unsafe "socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import CALLCONV unsafe "bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "accept"
  c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt

foreign import CALLCONV unsafe "send"
  c_send :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import CALLCONV unsafe "sendto"
  c_sendto :: CInt -> Ptr CChar -> CSize -> CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import CALLCONV unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import CALLCONV unsafe "recvfrom"
  c_recvfrom :: CInt -> Ptr CChar -> CSize -> CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

-----------------------------------------------------------------------------
-- Support for thread-safe blocking operations in GHC.

#if defined(__GLASGOW_HASKELL__) && !(defined(HAVE_WINSOCK_H) && !defined(cygwin32_TARGET_OS))


{-# SPECIALISE 
    throwErrnoIfMinus1Retry_mayBlock
	 :: String -> IO CInt -> IO CInt -> IO CInt #-}
throwErrnoIfMinus1Retry_mayBlock :: Num a => String -> IO a -> IO a -> IO a
throwErrnoIfMinus1Retry_mayBlock name on_block act = do
    res <- act
    if res == -1
        then do
            err <- getErrno
            if err == eINTR
                then throwErrnoIfMinus1Retry_mayBlock name on_block act
	        else if err == eWOULDBLOCK || err == eAGAIN
		        then on_block
                        else throwErrno name
        else return res

throwErrnoIfMinus1Retry_repeatOnBlock :: Num a => String -> IO b -> IO a -> IO a
throwErrnoIfMinus1Retry_repeatOnBlock name on_block act = do
  throwErrnoIfMinus1Retry_mayBlock name (on_block >> repeat) act
  where repeat = throwErrnoIfMinus1Retry_repeatOnBlock name on_block act

throwSocketErrorIfMinus1Retry name act = throwErrnoIfMinus1Retry name act

throwSocketErrorIfMinus1_ :: Num a => String -> IO a -> IO ()
throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_
#else

throwErrnoIfMinus1Retry_mayBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

throwErrnoIfMinus1Retry_repeatOnBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

throwSocketErrorIfMinus1_ :: Num a => String -> IO a -> IO ()
throwSocketErrorIfMinus1_ name act = do
  throwSocketErrorIfMinus1Retry name act
  return ()

# if defined(HAVE_WINSOCK_H) && !defined(cygwin32_TARGET_OS)
throwSocketErrorIfMinus1Retry name act = do
  r <- act
  if (r == -1) 
   then do
    rc   <- c_getLastError
    case rc of
      10093 -> do -- WSANOTINITIALISED
        withSocketsDo (return ())
	r <- act
	if (r == -1)
	 then (c_getLastError >>= throwSocketError name)
	 else return r
      _ -> throwSocketError name rc
   else return r

throwSocketError name rc = do
    pstr <- c_getWSError rc
    str  <- peekCString pstr
#  if __GLASGOW_HASKELL__
    ioError (IOError Nothing OtherError name str Nothing)
#  else    
    ioError (userError (name ++ ": socket error - " ++ str))
#  endif
foreign import CALLCONV unsafe "WSAGetLastError"
  c_getLastError :: IO CInt

foreign import ccall unsafe "getWSErrorDescr"
  c_getWSError :: CInt -> IO (Ptr CChar)


# else 
throwSocketErrorIfMinus1Retry name act = throwErrnoIfMinus1Retry name act
# endif
#endif /* __GLASGOW_HASKELL */

