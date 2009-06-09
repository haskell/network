{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
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

-- NOTE: ##, we want this interpreted when compiling the .hs, not by hsc2hs.
##include "Typeable.h"

#if defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS)
#define WITH_WINSOCK  1
#endif

#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
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
#if defined(IPV6_SOCKET_SUPPORT)
    HostAddress6,
    FlowInfo,
    ScopeID,
#endif
    ShutdownCmd(..),
    ProtocolNumber,
    defaultProtocol,        -- :: ProtocolNumber
    PortNumber(..),
	-- PortNumber is used non-abstractly in Network.BSD.  ToDo: remove
	-- this use and make the type abstract.

    -- * Address operations

    HostName,
    ServiceName,

#if defined(IPV6_SOCKET_SUPPORT)
    AddrInfo(..),

    AddrInfoFlag(..),
    addrInfoFlagImplemented,-- :: AddrInfoFlag -> Bool

    defaultHints,           -- :: AddrInfo

    getAddrInfo,            -- :: Maybe AddrInfo -> Maybe HostName -> Maybe ServiceName -> IO [AddrInfo]

    NameInfoFlag(..),

    getNameInfo,            -- :: [NameInfoFlag] -> Bool -> Bool -> SockAddr -> IO (Maybe HostName, Maybe ServiceName)
#endif

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

#ifdef HAVE_STRUCT_UCRED
	-- get the credentials of our domain socket peer.
    getPeerCred,         -- :: Socket -> IO (CUInt{-pid-}, CUInt{-uid-}, CUInt{-gid-})
#endif

    socketPort,		-- :: Socket -> IO PortNumber

    socketToHandle,	-- :: Socket -> IOMode -> IO Handle

    sendTo,		-- :: Socket -> String -> SockAddr -> IO Int
    sendBufTo,          -- :: Socket -> Ptr a -> Int -> SockAddr -> IO Int

    recvFrom,		-- :: Socket -> Int -> IO (String, Int, SockAddr)
    recvBufFrom,        -- :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
    
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
#if defined(IPV6_SOCKET_SUPPORT)
    iN6ADDR_ANY,	-- :: HostAddress6
#endif
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
import Hugs.Prelude ( IOException(..), IOErrorType(..) )
import Hugs.IO ( openFd )

{-# CFILES cbits/HsNet.c #-}
# if HAVE_STRUCT_MSGHDR_MSG_CONTROL || HAVE_STRUCT_MSGHDR_MSG_ACCRIGHTS
{-# CFILES cbits/ancilData.c #-}
# endif
# if defined(HAVE_WINSOCK_H) && !defined(__CYGWIN__)
{-# CFILES cbits/initWinSock.c cbits/winSockErr.c #-}
# endif
#endif

import Data.Bits
import Data.List (foldl')
import Data.Word ( Word8, Word16, Word32 )
import Foreign.Ptr ( Ptr, castPtr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Foreign.C.Error
import Foreign.C.String ( CString, withCString, peekCString, peekCStringLen, castCharToCChar )
import Foreign.C.Types ( CInt, CUInt, CChar, CSize )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray, pokeArray0 )
import Foreign.Marshal.Utils ( maybeWith, with )

import System.IO
import Control.Monad ( liftM, when )
import Data.Ratio ( (%) )

import qualified Control.Exception
import Control.Concurrent.MVar
import Data.Typeable
import System.IO.Error

#ifdef __GLASGOW_HASKELL__
import GHC.Conc		(threadWaitRead, threadWaitWrite)
# if defined(mingw32_HOST_OS)
import GHC.Conc		( asyncDoProc )
import Foreign( FunPtr )
# endif
import GHC.Handle
import GHC.IOBase
import qualified System.Posix.Internals
#else
import System.IO.Unsafe	(unsafePerformIO)
#endif

import Network.Socket.Internal

type HostName       = String
type ServiceName    = String

-- ----------------------------------------------------------------------------
-- On Windows, our sockets are not put in non-blocking mode (non-blocking
-- is not supported for regular file descriptors on Windows, and it would
-- be a pain to support it only for sockets).  So there are two cases:
--
--  - the threaded RTS uses safe calls for socket operations to get
--    non-blocking I/O, just like the rest of the I/O library
--
--  - with the non-threaded RTS, only some operations on sockets will be
--    non-blocking.  Reads and writes go through the normal async I/O
--    system.  accept() uses asyncDoProc so is non-blocking.  A handful
--    of others (recvFrom, sendFd, recvFd) will block all threads - if this
--    is a problem, -threaded is the workaround.
--
##if defined(mingw32_HOST_OS)
##define SAFE_ON_WIN safe
##else
##define SAFE_ON_WIN unsafe
##endif

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
  | ConvertedToHandle   -- is now a Handle, don't touch
  | Closed		-- sClose 
    deriving (Eq, Show)

INSTANCE_TYPEABLE0(SocketStatus,socketStatusTc,"SocketStatus")

data Socket
  = MkSocket
	    CInt	         -- File Descriptor
	    Family				  
	    SocketType				  
	    ProtocolNumber	 -- Protocol Number
	    (MVar SocketStatus)  -- Status Flag

INSTANCE_TYPEABLE0(Socket,socketTc,"Socket")

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

-- | This is the default protocol for a given service.
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

----------------------------------------------------------------------------
-- Port Numbers

INSTANCE_TYPEABLE0(PortNumber,portNumberTc,"PortNumber")

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

INSTANCE_TYPEABLE0(SockAddr,sockAddrTc,"SockAddr")

instance Show SockAddr where
#if defined(DOMAIN_SOCKET_SUPPORT)
  showsPrec _ (SockAddrUnix str) = showString str
#endif
  showsPrec _ (SockAddrInet port ha)
   = showString (unsafePerformIO (inet_ntoa ha)) 
   . showString ":"
   . shows port
#if defined(IPV6_SOCKET_SUPPORT)
  showsPrec _ addr@(SockAddrInet6 port _ _ _)
   = showChar '['
   . showString (unsafePerformIO $
                 fst `liftM` getNameInfo [NI_NUMERICHOST] True False addr >>=
                 maybe (fail "showsPrec: impossible internal error") return)
   . showString "]:"
   . shows port
#endif

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
socketPair :: Family 	          -- Family Name (usually AF_INET or AF_INET6)
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
#if !(defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS))
	       	       err <- getErrno
		       case () of
			 _ | err == eINTR       -> connectLoop
			 _ | err == eINPROGRESS -> connectBlocked
--			 _ | err == eAGAIN      -> connectBlocked
			 otherwise              -> throwSocketError "connect"
#else
		       rc <- c_getLastError
		       case rc of
		         10093 -> do -- WSANOTINITIALISED
			   withSocketsDo (return ())
	       	           r <- c_connect s p_addr (fromIntegral sz)
	       	           if r == -1
			    then throwSocketError "connect"
			    else return r
			 _ -> throwSocketError "connect"
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
     let sz = sizeOfSockAddrByFamily family
     allocaBytes sz $ \ sockaddr -> do
#if defined(mingw32_HOST_OS) && defined(__GLASGOW_HASKELL__)
     new_sock <-
	if threaded 
	   then with (fromIntegral sz) $ \ ptr_len ->
		  throwErrnoIfMinus1Retry "Network.Socket.accept" $
		    c_accept_safe s sockaddr ptr_len
	   else do
     		paramData <- c_newAcceptParams s (fromIntegral sz) sockaddr
     		rc        <- asyncDoProc c_acceptDoProc paramData
     		new_sock  <- c_acceptNewSock    paramData
     		c_free paramData
     		when (rc /= 0)
     		     (ioError (errnoToIOError "Network.Socket.accept" (Errno (fromIntegral rc)) Nothing Nothing))
		return new_sock
#else 
     with (fromIntegral sz) $ \ ptr_len -> do
     new_sock <- 
# if !defined(__HUGS__)
                 throwSocketErrorIfMinus1RetryMayBlock "accept"
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

#if defined(mingw32_HOST_OS) && !defined(__HUGS__)
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

-- | NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
sendTo :: Socket	-- (possibly) bound/connected Socket
       -> String	-- Data to send
       -> SockAddr
       -> IO Int	-- Number of Bytes sent

sendTo sock xs addr = do
 withCString xs $ \str -> do
   sendBufTo sock str (length xs) addr

sendBufTo :: Socket	      -- (possibly) bound/connected Socket
          -> Ptr a -> Int     -- Data to send
          -> SockAddr
          -> IO Int	      -- Number of Bytes sent

sendBufTo (MkSocket s _family _stype _protocol status) ptr nbytes addr = do
 withSockAddr addr $ \p_addr sz -> do
   liftM fromIntegral $
#if !defined(__HUGS__)
     throwSocketErrorIfMinus1RetryMayBlock "sendTo"
	(threadWaitWrite (fromIntegral s)) $
#endif
	c_sendto s ptr (fromIntegral $ nbytes) 0{-flags-} 
			p_addr (fromIntegral sz)

-- | NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock nbytes =
  allocaBytes nbytes $ \ptr -> do
    (len, sockaddr) <- recvBufFrom sock ptr nbytes
    str <- peekCStringLen (ptr, len)
    return (str, len, sockaddr)

recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom sock@(MkSocket s family _stype _protocol status) ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvFrom")
 | otherwise   = 
    withNewSockAddr family $ \ptr_addr sz -> do
      alloca $ \ptr_len -> do
      	poke ptr_len (fromIntegral sz)
        len <- 
#if !defined(__HUGS__)
	       throwSocketErrorIfMinus1RetryMayBlock "recvFrom"
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
           return (len', sockaddr)

-----------------------------------------------------------------------------
-- send & recv

send :: Socket	-- Bound/Connected Socket
     -> String	-- Data to send
     -> IO Int	-- Number of Bytes sent
send (MkSocket s _family _stype _protocol status) xs = do
 let len = length xs
 withCString xs $ \str -> do
   liftM fromIntegral $
#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)
      writeRawBufferPtr "Network.Socket.send" (fromIntegral s) True str 0
		(fromIntegral len)
#else
# if !defined(__HUGS__)
     throwSocketErrorIfMinus1RetryMayBlock "send"
	(threadWaitWrite (fromIntegral s)) $
# endif
	c_send s str (fromIntegral len) 0{-flags-} 
#endif

recv :: Socket -> Int -> IO String
recv sock l = recvLen sock l >>= \ (s,_) -> return s

recvLen :: Socket -> Int -> IO (String, Int)
recvLen sock@(MkSocket s _family _stype _protocol status) nbytes 
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recv")
 | otherwise   = do
     allocaBytes nbytes $ \ptr -> do
        len <- 
#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)
	  readRawBufferPtr "Network.Socket.recvLen" (fromIntegral s) True ptr 0
		 (fromIntegral nbytes)
#else
# if !defined(__HUGS__)
	       throwSocketErrorIfMinus1RetryMayBlock "recv"
        	   (threadWaitRead (fromIntegral s)) $
# endif
        	   c_recv s ptr (fromIntegral nbytes) 0{-flags-} 
#endif
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
#if defined(IPV6_SOCKET_SUPPORT)
socketPort sock@(MkSocket _ AF_INET6 _ _ _) = do
    (SockAddrInet6 port _ _ _) <- getSocketName sock
    return port
#endif
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

INSTANCE_TYPEABLE0(SocketOption,socketOptionTc,"SocketOption")

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


#ifdef HAVE_STRUCT_UCRED
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
  throwSocketErrorIfMinus1RetryMayBlock "sendFd"
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
    throwSocketErrorIfMinus1RetryMayBlock "recvFd" 
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
   throwSocketErrorIfMinus1RetryMayBlock "sendAncillary"
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
        throwSocketErrorIfMinus1RetryMayBlock "recvAncillary" 
            (threadWaitRead (fromIntegral fd)) $
#endif
	    c_recvAncillary fd ptr_lev ptr_ty (fromIntegral flags) ptr_pData ptr_len
      len <- fromIntegral `liftM` peek ptr_len
      lev <- fromIntegral `liftM` peek ptr_lev
      ty  <- fromIntegral `liftM` peek ptr_ty
      pD  <- peek ptr_pData
      return (lev,ty,pD, len)
foreign import ccall SAFE_ON_WIN "sendAncillary"
  c_sendAncillary :: CInt -> CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall SAFE_ON_WIN "recvAncillary"
  c_recvAncillary :: CInt -> Ptr CInt -> Ptr CInt -> CInt -> Ptr (Ptr a) -> Ptr CInt -> IO CInt

foreign import ccall SAFE_ON_WIN "sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall SAFE_ON_WIN "recvFd" c_recvFd :: CInt -> IO CInt

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
unpackSocketType:: CInt -> SocketType

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
#ifdef AF_NETROM
	AF_NETROM -> #const AF_NETROM
#endif
#ifdef AF_BRIDGE
	AF_BRIDGE -> #const AF_BRIDGE
#endif
#ifdef AF_ATMPVC
	AF_ATMPVC -> #const AF_ATMPVC
#endif
#ifdef AF_ROSE
	AF_ROSE -> #const AF_ROSE
#endif
#ifdef AF_NETBEUI
	AF_NETBEUI -> #const AF_NETBEUI
#endif
#ifdef AF_SECURITY
	AF_SECURITY -> #const AF_SECURITY
#endif
#ifdef AF_PACKET
	AF_PACKET -> #const AF_PACKET
#endif
#ifdef AF_ASH
	AF_ASH -> #const AF_ASH
#endif
#ifdef AF_ECONET
	AF_ECONET -> #const AF_ECONET
#endif
#ifdef AF_ATMSVC
	AF_ATMSVC -> #const AF_ATMSVC
#endif
#ifdef AF_IRDA
	AF_IRDA -> #const AF_IRDA
#endif
#ifdef AF_PPPOX
	AF_PPPOX -> #const AF_PPPOX
#endif
#ifdef AF_WANPIPE
	AF_WANPIPE -> #const AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
	AF_BLUETOOTH -> #const AF_BLUETOOTH
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
#if defined(AF_IPX) && (!defined(AF_NS) || AF_NS != AF_IPX)
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
#ifdef AF_NETROM
	(#const AF_NETROM) -> AF_NETROM
#endif
#ifdef AF_BRIDGE
	(#const AF_BRIDGE) -> AF_BRIDGE
#endif
#ifdef AF_ATMPVC
	(#const AF_ATMPVC) -> AF_ATMPVC
#endif
#ifdef AF_ROSE
	(#const AF_ROSE) -> AF_ROSE
#endif
#ifdef AF_NETBEUI
	(#const AF_NETBEUI) -> AF_NETBEUI
#endif
#ifdef AF_SECURITY
	(#const AF_SECURITY) -> AF_SECURITY
#endif
#ifdef AF_PACKET
	(#const AF_PACKET) -> AF_PACKET
#endif
#ifdef AF_ASH
	(#const AF_ASH) -> AF_ASH
#endif
#ifdef AF_ECONET
	(#const AF_ECONET) -> AF_ECONET
#endif
#ifdef AF_ATMSVC
	(#const AF_ATMSVC) -> AF_ATMSVC
#endif
#ifdef AF_IRDA
	(#const AF_IRDA) -> AF_IRDA
#endif
#ifdef AF_PPPOX
	(#const AF_PPPOX) -> AF_PPPOX
#endif
#ifdef AF_WANPIPE
	(#const AF_WANPIPE) -> AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
	(#const AF_BLUETOOTH) -> AF_BLUETOOTH
#endif
	unknown -> error ("Network.Socket.unpackFamily: unknown address " ++
                          "family " ++ show unknown)

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
	
INSTANCE_TYPEABLE0(SocketType,socketTypeTc,"SocketType")

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

unpackSocketType t = case t of
	0 -> NoSocketType
#ifdef SOCK_STREAM
	(#const SOCK_STREAM) -> Stream
#endif
#ifdef SOCK_DGRAM
	(#const SOCK_DGRAM) -> Datagram
#endif
#ifdef SOCK_RAW
	(#const SOCK_RAW) -> Raw
#endif
#ifdef SOCK_RDM
	(#const SOCK_RDM) -> RDM
#endif
#ifdef SOCK_SEQPACKET
	(#const SOCK_SEQPACKET) -> SeqPacket
#endif

-- ---------------------------------------------------------------------------
-- Utility Functions

aNY_PORT :: PortNumber 
aNY_PORT = 0

-- | The IPv4 wild card address.

iNADDR_ANY :: HostAddress
iNADDR_ANY = htonl (#const INADDR_ANY)

#if defined(IPV6_SOCKET_SUPPORT)
-- | The IPv6 wild card address.

iN6ADDR_ANY :: HostAddress6
iN6ADDR_ANY = (0, 0, 0, 0)
#endif

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

INSTANCE_TYPEABLE0(ShutdownCmd,shutdownCmdTc,"ShutdownCmd")

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry "shutdown" (c_shutdown s (sdownCmdToInt stype))
  return ()

-- -----------------------------------------------------------------------------

-- | Closes a socket
sClose	 :: Socket -> IO ()
sClose (MkSocket s _ _ _ socketStatus) = do 
 modifyMVar_ socketStatus $ \ status ->
   case status of
     ConvertedToHandle ->
	 ioError (userError ("sClose: converted to a Handle, use hClose instead"))
     Closed ->
	 return status
     _ -> c_close s >> return Closed

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

-- | turns a Socket into an 'Handle'. By default, the new handle is
-- unbuffered. Use 'System.IO.hSetBuffering' to change the buffering.
--
-- Note that since a 'Handle' is automatically closed by a finalizer
-- when it is no longer referenced, you should avoid doing any more
-- operations on the 'Socket' after calling 'socketToHandle'.  To
-- close the 'Socket' after 'socketToHandle', call 'System.IO.hClose'
-- on the 'Handle'.

#ifndef __PARALLEL_HASKELL__
socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle s@(MkSocket fd _ _ _ socketStatus) mode = do
 modifyMVar socketStatus $ \ status ->
    if status == ConvertedToHandle
	then ioError (userError ("socketToHandle: already a Handle"))
	else do
# if __GLASGOW_HASKELL__ >= 608
    h <- fdToHandle' (fromIntegral fd) (Just System.Posix.Internals.Stream) True (show s) mode True{-bin-}
# elif __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 608
    h <- openFd (fromIntegral fd) (Just System.Posix.Internals.Stream) True (show s) mode True{-bin-}
# elif defined(__HUGS__)
    h <- openFd (fromIntegral fd) True{-is a socket-} mode True{-bin-}
# endif
    return (ConvertedToHandle, h)
#else
socketToHandle (MkSocket s family stype protocol status) m =
  error "socketToHandle not implemented in a parallel setup"
#endif

-- | Pack a list of values into a bitmask.  The possible mappings from
-- value to bit-to-set are given as the first argument.  We assume
-- that each value can cause exactly one bit to be set; unpackBits will
-- break if this property is not true.

packBits :: (Eq a, Bits b) => [(a, b)] -> [a] -> b

packBits mapping xs = foldl' pack 0 mapping
    where pack acc (k, v) | k `elem` xs = acc .|. v
                          | otherwise   = acc

-- | Unpack a bitmask into a list of values.

unpackBits :: Bits b => [(a, b)] -> b -> [a]

-- Be permissive and ignore unknown bit values. At least on OS X,
-- getaddrinfo returns an ai_flags field with bits set that have no
-- entry in <netdb.h>.
unpackBits [] _    = []
unpackBits ((k,v):xs) r
    | r .&. v /= 0 = k : unpackBits xs (r .&. complement v)
    | otherwise    = unpackBits xs r

-----------------------------------------------------------------------------
-- Address and service lookups

#if defined(IPV6_SOCKET_SUPPORT)

-- | Flags that control the querying behaviour of 'getAddrInfo'.
data AddrInfoFlag
    = AI_ADDRCONFIG
    | AI_ALL
    | AI_CANONNAME
    | AI_NUMERICHOST
    | AI_NUMERICSERV
    | AI_PASSIVE
    | AI_V4MAPPED
    deriving (Eq, Read, Show)

INSTANCE_TYPEABLE0(AddrInfoFlag,addrInfoFlagTc,"AddrInfoFlag")

aiFlagMapping :: [(AddrInfoFlag, CInt)]

aiFlagMapping =
    [
#if HAVE_DECL_AI_ADDRCONFIG
     (AI_ADDRCONFIG, #const AI_ADDRCONFIG),
#else
     (AI_ADDRCONFIG, 0),
#endif
#if HAVE_DECL_AI_ALL
     (AI_ALL, #const AI_ALL),
#else
     (AI_ALL, 0),
#endif
     (AI_CANONNAME, #const AI_CANONNAME),
     (AI_NUMERICHOST, #const AI_NUMERICHOST),
#if HAVE_DECL_AI_NUMERICSERV
     (AI_NUMERICSERV, #const AI_NUMERICSERV),
#else
     (AI_NUMERICSERV, 0),
#endif
     (AI_PASSIVE, #const AI_PASSIVE),
#if HAVE_DECL_AI_V4MAPPED
     (AI_V4MAPPED, #const AI_V4MAPPED)
#else
     (AI_V4MAPPED, 0)
#endif
    ]

-- | Indicate whether the given 'AddrInfoFlag' will have any effect on
-- this system.
addrInfoFlagImplemented :: AddrInfoFlag -> Bool
addrInfoFlagImplemented f = packBits aiFlagMapping [f] /= 0

data AddrInfo =
    AddrInfo {
        addrFlags :: [AddrInfoFlag],
        addrFamily :: Family,
        addrSocketType :: SocketType,
        addrProtocol :: ProtocolNumber,
        addrAddress :: SockAddr,
        addrCanonName :: Maybe String
        }
    deriving (Eq, Show)

INSTANCE_TYPEABLE0(AddrInfo,addrInfoTc,"AddrInfo")

instance Storable AddrInfo where
    sizeOf    _ = #const sizeof(struct addrinfo)
    alignment _ = alignment (undefined :: CInt)

    peek p = do
	ai_flags <- (#peek struct addrinfo, ai_flags) p
	ai_family <- (#peek struct addrinfo, ai_family) p
	ai_socktype <- (#peek struct addrinfo, ai_socktype) p
	ai_protocol <- (#peek struct addrinfo, ai_protocol) p
        ai_addr <- (#peek struct addrinfo, ai_addr) p >>= peekSockAddr
	ai_canonname_ptr <- (#peek struct addrinfo, ai_canonname) p

        ai_canonname <- if ai_canonname_ptr == nullPtr
                        then return Nothing
                        else liftM Just $ peekCString ai_canonname_ptr
                             
        return (AddrInfo
                {
                 addrFlags = unpackBits aiFlagMapping ai_flags,
                 addrFamily = unpackFamily ai_family,
                 addrSocketType = unpackSocketType ai_socktype,
                 addrProtocol = ai_protocol,
                 addrAddress = ai_addr,
                 addrCanonName = ai_canonname
                })

    poke p (AddrInfo flags family socketType protocol _ _) = do
        (#poke struct addrinfo, ai_flags) p (packBits aiFlagMapping flags)
        (#poke struct addrinfo, ai_family) p (packFamily family)
        (#poke struct addrinfo, ai_socktype) p (packSocketType socketType)
        (#poke struct addrinfo, ai_protocol) p protocol

        -- stuff below is probably not needed, but let's zero it for safety

        (#poke struct addrinfo, ai_addrlen) p (0::CSize)
        (#poke struct addrinfo, ai_addr) p nullPtr
        (#poke struct addrinfo, ai_canonname) p nullPtr
        (#poke struct addrinfo, ai_next) p nullPtr

data NameInfoFlag
    = NI_DGRAM
    | NI_NAMEREQD
    | NI_NOFQDN
    | NI_NUMERICHOST
    | NI_NUMERICSERV
    deriving (Eq, Read, Show)

INSTANCE_TYPEABLE0(NameInfoFlag,nameInfoFlagTc,"NameInfoFlag")

niFlagMapping :: [(NameInfoFlag, CInt)]

niFlagMapping = [(NI_DGRAM, #const NI_DGRAM),
                 (NI_NAMEREQD, #const NI_NAMEREQD),
                 (NI_NOFQDN, #const NI_NOFQDN),
                 (NI_NUMERICHOST, #const NI_NUMERICHOST),
                 (NI_NUMERICSERV, #const NI_NUMERICSERV)]

-- | Default hints for address lookup with 'getAddrInfo'.  The values
-- of the 'addrAddress' and 'addrCanonName' fields are 'undefined',
-- and are never inspected by 'getAddrInfo'.

defaultHints :: AddrInfo

defaultHints = AddrInfo {
                         addrFlags = [],
                         addrFamily = AF_UNSPEC,
                         addrSocketType = NoSocketType,
                         addrProtocol = defaultProtocol,
                         addrAddress = undefined,
                         addrCanonName = undefined
                        }

-- | Resolve a host or service name to one or more addresses.
-- The 'AddrInfo' values that this function returns contain 'SockAddr'
-- values that you can pass directly to 'connect' or
-- 'bindSocket'.
--
-- This function is protocol independent.  It can return both IPv4 and
-- IPv6 address information.
--
-- The 'AddrInfo' argument specifies the preferred query behaviour,
-- socket options, or protocol.  You can override these conveniently
-- using Haskell's record update syntax on 'defaultHints', for example
-- as follows:
--
-- @
--   myHints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
-- @
--
-- Values for 'addrFlags' control query behaviour.  The supported
-- flags are as follows:
--
--   [@AI_PASSIVE@] If no 'HostName' value is provided, the network
--     address in each 'SockAddr'
--     will be left as a "wild card", i.e. as either 'iNADDR_ANY'
--     or 'iN6ADDR_ANY'.  This is useful for server applications that
--     will accept connections from any client.
--
--   [@AI_CANONNAME@] The 'addrCanonName' field of the first returned
--     'AddrInfo' will contain the "canonical name" of the host.
--
--   [@AI_NUMERICHOST@] The 'HostName' argument /must/ be a numeric
--     address in string form, and network name lookups will not be
--     attempted.
-- 
-- /Note/: Although the following flags are required by RFC 3493, they
-- may not have an effect on all platforms, because the underlying
-- network stack may not support them.  To see whether a flag from the
-- list below will have any effect, call 'addrInfoFlagImplemented'.
--
--   [@AI_NUMERICSERV@] The 'ServiceName' argument /must/ be a port
--     number in string form, and service name lookups will not be
--     attempted.
--
--   [@AI_ADDRCONFIG@] The list of returned 'AddrInfo' values will
--     only contain IPv4 addresses if the local system has at least
--     one IPv4 interface configured, and likewise for IPv6.
--
--   [@AI_V4MAPPED@] If an IPv6 lookup is performed, and no IPv6
--     addresses are found, IPv6-mapped IPv4 addresses will be
--     returned.
--
--   [@AI_ALL@] If 'AI_ALL' is specified, return all matching IPv6 and
--     IPv4 addresses.  Otherwise, this flag has no effect.
--     
-- You must provide a 'Just' value for at least one of the 'HostName'
-- or 'ServiceName' arguments.  'HostName' can be either a numeric
-- network address (dotted quad for IPv4, colon-separated hex for
-- IPv6) or a hostname.  In the latter case, its addresses will be
-- looked up unless 'AI_NUMERICHOST' is specified as a hint.  If you
-- do not provide a 'HostName' value /and/ do not set 'AI_PASSIVE' as
-- a hint, network addresses in the result will contain the address of
-- the loopback interface.
--
-- If the query fails, this function throws an IO exception instead of
-- returning an empty list.  Otherwise, it returns a non-empty list
-- of 'AddrInfo' values.
--
-- There are several reasons why a query might result in several
-- values.  For example, the queried-for host could be multihomed, or
-- the service might be available via several protocols.
--
-- Note: the order of arguments is slightly different to that defined
-- for @getaddrinfo@ in RFC 2553.  The 'AddrInfo' parameter comes first
-- to make partial application easier.
--
-- Example:
-- @
--   let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
--   addrs <- getAddrInfo (Just hints) (Just "www.haskell.org") (Just "http")
--   let addr = head addrs
--   sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--   connect sock (addrAddress addr)
-- @

getAddrInfo :: Maybe AddrInfo -- ^ preferred socket type or protocol
            -> Maybe HostName -- ^ host name to look up
            -> Maybe ServiceName -- ^ service name to look up
            -> IO [AddrInfo] -- ^ resolved addresses, with "best" first

getAddrInfo hints node service =
  maybeWith withCString node $ \c_node ->
    maybeWith withCString service $ \c_service ->
      maybeWith with hints $ \c_hints ->
        alloca $ \ptr_ptr_addrs -> do
          ret <- c_getaddrinfo c_node c_service c_hints ptr_ptr_addrs
          case ret of
            0 -> do ptr_addrs <- peek ptr_ptr_addrs
                    ais <- followAddrInfo ptr_addrs
                    c_freeaddrinfo ptr_addrs
                    return ais
            _ -> do err <- gai_strerror ret
                    ioError (ioeSetErrorString
                             (mkIOError NoSuchThing "getAddrInfo" Nothing
                              Nothing) err)

followAddrInfo :: Ptr AddrInfo -> IO [AddrInfo]

followAddrInfo ptr_ai | ptr_ai == nullPtr = return []
                      | otherwise = do
    a <- peek ptr_ai
    as <- (#peek struct addrinfo, ai_next) ptr_ai >>= followAddrInfo
    return (a:as)

foreign import ccall safe "hsnet_getaddrinfo"
    c_getaddrinfo :: CString -> CString -> Ptr AddrInfo -> Ptr (Ptr AddrInfo)
                  -> IO CInt

foreign import ccall safe "hsnet_freeaddrinfo"
    c_freeaddrinfo :: Ptr AddrInfo -> IO ()

gai_strerror :: CInt -> IO String

#ifdef HAVE_GAI_STRERROR
gai_strerror n = c_gai_strerror n >>= peekCString

foreign import ccall safe "gai_strerror"
    c_gai_strerror :: CInt -> IO CString
#else
gai_strerror n = return ("error " ++ show n)
#endif

withCStringIf :: Bool -> Int -> (CSize -> CString -> IO a) -> IO a
withCStringIf False _ f = f 0 nullPtr
withCStringIf True n f = allocaBytes n (f (fromIntegral n))
                    
-- | Resolve an address to a host or service name.
-- This function is protocol independent.
--
-- The list of 'NameInfoFlag' values controls query behaviour.  The
-- supported flags are as follows:
--
--   [@NI_NOFQDN@] If a host is local, return only the
--     hostname part of the FQDN.
--
--   [@NI_NUMERICHOST@] The name of the host is not
--     looked up.  Instead, a numeric representation of the host's
--     address is returned.  For an IPv4 address, this will be a
--     dotted-quad string.  For IPv6, it will be colon-separated
--     hexadecimal.
--
--   [@NI_NUMERICSERV@] The name of the service is not
--     looked up.  Instead, a numeric representation of the
--     service is returned.
--
--   [@NI_NAMEREQD@] If the hostname cannot be looked up, an IO error
--     is thrown.
--
--   [@NI_DGRAM@] Resolve a datagram-based service name.  This is
--     required only for the few protocols that have different port
--     numbers for their datagram-based versions than for their
--     stream-based versions.
--
-- Hostname and service name lookups can be expensive.  You can
-- specify which lookups to perform via the two 'Bool' arguments.  If
-- one of these is 'False', the corresponding value in the returned
-- tuple will be 'Nothing', and no lookup will be performed.
--
-- If a host or service's name cannot be looked up, then the numeric
-- form of the address or service will be returned.
--
-- If the query fails, this function throws an IO exception.
--
-- Example:
-- @
--   (hostName, _) <- getNameInfo [] True False myAddress
-- @

getNameInfo :: [NameInfoFlag] -- ^ flags to control lookup behaviour
            -> Bool -- ^ whether to look up a hostname
            -> Bool -- ^ whether to look up a service name
            -> SockAddr -- ^ the address to look up
            -> IO (Maybe HostName, Maybe ServiceName)

getNameInfo flags doHost doService addr =
  withCStringIf doHost (#const NI_MAXHOST) $ \c_hostlen c_host ->
    withCStringIf doService (#const NI_MAXSERV) $ \c_servlen c_serv -> do
      withSockAddr addr $ \ptr_addr sz -> do
        ret <- c_getnameinfo ptr_addr (fromIntegral sz) c_host c_hostlen
                             c_serv c_servlen (packBits niFlagMapping flags)
        case ret of
          0 -> do
            let peekIf doIf c_val = if doIf
                                     then liftM Just $ peekCString c_val
                                     else return Nothing
            host <- peekIf doHost c_host
            serv <- peekIf doService c_serv
            return (host, serv)
          _ -> do err <- gai_strerror ret
                  ioError (ioeSetErrorString
                           (mkIOError NoSuchThing "getNameInfo" Nothing 
                            Nothing) err)

foreign import ccall safe "hsnet_getnameinfo"
    c_getnameinfo :: Ptr SockAddr -> CInt{-CSockLen???-} -> CString -> CSize -> CString
                  -> CSize -> CInt -> IO CInt
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
#ifdef __GLASGOW_HASKELL__
				    InvalidArgument
#else
				    IllegalOperation
#endif
                                    loc Nothing Nothing) "non-positive length"

mkEOFError :: String -> IOError
mkEOFError loc = ioeSetErrorString (mkIOError EOF loc Nothing Nothing) "end of file"

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
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import CALLCONV unsafe "listen"
  c_listen :: CInt -> CInt -> IO CInt

#ifdef __GLASGOW_HASKELL__
foreign import ccall "rtsSupportsBoundThreads" threaded :: Bool
#endif

foreign import CALLCONV unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "sendto"
  c_sendto :: CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import CALLCONV unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import CALLCONV SAFE_ON_WIN "recvfrom"
  c_recvfrom :: CInt -> Ptr a -> CSize -> CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getpeername"
  c_getpeername :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "getsockname"
  c_getsockname :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
