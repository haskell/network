{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Johan Tibell 2007-2011
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  johan.tibell@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).

#include "HsNet.h"

-- NOTE: ##, we want this interpreted when compiling the .hs, not by hsc2hs.
##include "Typeable.h"

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket
    (
    -- * Unicode issues
    -- $unicode

    -- * Types
      Socket(..)
    , Family(..)
    , SocketType(..)
    , SockAddr(..)
    , SocketStatus(..)
    , HostAddress
    , ProtocolNumber
    , defaultProtocol
    , PortID(..)
    , PortNumber
#if defined(IPV6_SOCKET_SUPPORT)
    -- ** IPv6 address components
    , HostAddress6
    , FlowInfo
    , ScopeID
#endif

    -- * Address operations
    , HostName
    , ServiceName
#if defined(IPV6_SOCKET_SUPPORT)
    , AddrInfo(..)
    , AddrInfoFlag(..)
    , addrInfoFlagImplemented
    , defaultHints
    , getAddrInfo
    , NameInfoFlag(..)
    , getNameInfo
#endif

    -- * Creating sockets
    , socket
#if defined(DOMAIN_SOCKET_SUPPORT)
    , socketPair
#endif

    -- * Socket operations
    -- ** High-level setup
    , connectTo
    , listenOn
    , accept
    -- ** Low-level setup
    , bind
    , connect
#ifdef HAVE_STRUCT_UCRED
    -- get the credentials of our domain socket peer.
    , getPeerCred
#endif
    , getPeerName
    , getSocketName
    , listen
    , ShutdownCmd(..)
    , shutdown
    , close
    , socketPort
    , toHandle

    -- ** Send data to a socket
    , send
    , sendAll
    , sendTo
    , sendBufTo
    , sendAllTo

    -- *** Vectored I/O
    -- $vectored
    , sendMany
    , sendManyTo

    -- ** Receive data from a socket
    , recv
    , recvBuf
    , recvFrom
    , recvBufFrom

    -- ** Predicates on sockets
    , sIsConnected
    , sIsBound
    , sIsListening
    , sIsReadable
    , sIsWritable

    -- ** Socket options
    , SocketOption(..)
    , getSocketOption
    , setSocketOption

    -- ** File descriptor transmission
#ifdef DOMAIN_SOCKET_SUPPORT
    , sendFd
    , recvFd
#endif

    -- * Special constants
    , aNY_PORT
    , iNADDR_ANY
#if defined(IPV6_SOCKET_SUPPORT)
    , iN6ADDR_ANY
#endif
    , sOMAXCONN
    , sOL_SOCKET
#ifdef SCM_RIGHTS
    , sCM_RIGHTS
#endif
    , maxListenQueue

    -- * Initialisation
    , withSocketsDo

    -- * Utility functions
    , inet_addr
    , inet_ntoa

    -- * Very low level operations
    -- in case you ever want to get at the underlying file descriptor..
    , fdSocket
    , mkSocket
    ) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar,
                                readMVar)
import qualified Control.Exception as Exception
import Control.Monad (liftM, when)
import Data.Bits (Bits, (.|.), (.&.), complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.List (foldl')
import Data.Typeable (Typeable)
import Data.Word (Word8, Word32)
import Foreign.C.Error (Errno(..), eINPROGRESS, eINTR, errnoToIOError, getErrno,
                        throwErrnoIfMinus1, throwErrnoIfMinus1_,
                        throwErrnoIfMinus1Retry_)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CChar)
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CInt(..), CSize(..))
#else
import Foreign.C.Types (CInt, CSize)
#endif
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (maybeWith, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Network.Socket.ByteString.Internal hiding (mkInvalidRecvArgError)
import Network.Socket.Internal
import System.IO (Handle, IOMode, )
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.Mem.Weak (addFinalizer)

##if !MIN_VERSION_base(4,3,1)
import System.Posix.Types (Fd)
##endif

#ifdef HAVE_STRUCT_UCRED
import Foreign.C.Types (CUInt)
#endif

#if !defined(mingw32_HOST_OS)
import Control.Monad (zipWithM_)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (plusPtr)
import Network.Socket.ByteString.IOVec (IOVec(..))
import Network.Socket.ByteString.MsgHdr (MsgHdr(..))
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Conc (threadWaitRead, threadWaitWrite)
## if MIN_VERSION_base(4,3,1)
import GHC.Conc (closeFdWith)
## endif
# if defined(mingw32_HOST_OS)
import Foreign (FunPtr)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import GHC.Conc (asyncDoProc)
#  if __GLASGOW_HASKELL__ >= 611
import GHC.IO.FD
#  else
import GHC.Handle (readRawBufferPtr, writeRawBufferPtr)
#  endif
# endif
# if __GLASGOW_HASKELL__ >= 611
import GHC.IO (unsafePerformIO)
import GHC.IO.Exception (IOErrorType(EOF, InvalidArgument, NoSuchThing))
import qualified GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Handle.FD (fdToHandle')
# else
import GHC.Handle
import GHC.IOBase
# endif
import qualified System.Posix.Internals
#else
import System.IO.Unsafe (unsafePerformIO)
#endif

-- $unicode
--
-- The operating system socket API does not directly support Unicode,
-- and neither does this module.  All encoding/decoding must be
-- handled by the API user.

-- | Either a host name e.g., @\"haskell.org\"@ or a numeric host
-- address string consisting of a dotted decimal IPv4 address or an
-- IPv6 address e.g., @\"192.168.0.1\"@.
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
  -- Returned Status    Function called
  = NotConnected        -- socket
  | Bound               -- bind
  | Listening           -- listen
  | Connected           -- connect/accept
  | ConvertedToHandle   -- is now a Handle, don't touch
  | Closed              -- close
    deriving (Eq, Show, Typeable)

-- | A socket.
--
-- Since sockets are scarce system resources, you should manage them
-- manually: use 'close' or 'shutdown' to close a socket when you are
-- finished with it.
--
-- Every 'Socket' has a finalizer associated with it that may
-- eventually close the socket some time after it becomes unreachable.
-- Since finalizers are neither reliable nor timely, you should not
-- rely on this behaviour, as by doing so you may leave yourself open
-- to resource leaks.
data Socket
  = MkSocket
            CInt                 -- File Descriptor
            Family
            SocketType
            ProtocolNumber       -- Protocol Number
            (MVar SocketStatus)  -- Status Flag
  deriving Typeable

-- | Create a 'Socket' from the low-level description.
mkSocket :: CInt
         -> Family
         -> SocketType
         -> ProtocolNumber
         -> SocketStatus
         -> IO Socket
mkSocket fd fam sType pNum stat = do
  sock <- MkSocket fd fam sType pNum `liftM` newMVar stat
  addSocketFinalizer sock
  return sock

-- | Add a finalizer to a 'Socket'. Normally, the 'Socket' should be
-- explicitly closed before all references to it are garbage
-- collected. If it has not been explicitly closed, the finalizer
-- silently closes the 'Socket' once all references to it have been
-- lost.
addSocketFinalizer :: Socket -> IO ()
addSocketFinalizer sock@(MkSocket fd _fam _type _num mstat) =
    addFinalizer sock . modifyMVar_ mstat $ \stat ->
      case stat of
        s@ConvertedToHandle -> return s
        s@Closed            -> return s
        _                   -> do
          closeFdWith (\n -> closeFd (fromIntegral n) `catchIO` const (return ()))
                      (fromIntegral fd)
          return Closed

instance Eq Socket where
  (MkSocket _ _ _ _ m1) == (MkSocket _ _ _ _ m2) = m1 == m2

instance Show Socket where
  showsPrec _ (MkSocket fd _ _ _ _) =
        showString "<socket: " . shows fd . showString ">"


fdSocket :: Socket -> CInt
fdSocket (MkSocket fd _ _ _ _) = fd

type ProtocolNumber = CInt

-- | This is the default protocol for a given service.
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

-----------------------------------------------------------------------------
-- SockAddr

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

-- | Create a new socket using the given address family, socket type
-- and protocol number.  The address family is usually 'AF_INET',
-- 'AF_INET6', or 'AF_UNIX'.  The socket type is usually 'Stream' or
-- 'Datagram'.  The protocol number is usually 'defaultProtocol'.
socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket      -- Unconnected Socket
socket family stype protocol = do
    fd <- throwSocketErrorIfMinus1Retry "socket" $
                c_socket (packFamily family) (packSocketType stype) protocol
#if __GLASGOW_HASKELL__ < 611
    System.Posix.Internals.setNonBlockingFD fd
#else
    System.Posix.Internals.setNonBlockingFD fd True
#endif
    sock <- MkSocket fd family stype protocol `liftM` newMVar NotConnected
    addSocketFinalizer sock
    return sock

-- | Build a pair of connected socket objects using the given address
-- family, socket type, and protocol number.  Address family, socket
-- type, and protocol number are as for the 'socket' function above.
-- Availability: Unix.
#if defined(DOMAIN_SOCKET_SUPPORT)
socketPair :: Family              -- Family Name (usually AF_INET or AF_INET6)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
socketPair family stype protocol = do
   allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
    _ <- throwSocketErrorIfMinus1Retry "socketpair" $
                c_socketpair (packFamily family)
                             (packSocketType stype)
                             protocol fdArr
    [fd1,fd2] <- peekArray 2 fdArr
    s1 <- create fd1
    s2 <- create fd2
    return (s1,s2)
  where
    create fd = do
#if __GLASGOW_HASKELL__ < 611
       System.Posix.Internals.setNonBlockingFD fd
#else
       System.Posix.Internals.setNonBlockingFD fd True
#endif
       sock <- MkSocket fd family stype protocol `liftM` newMVar Connected
       addSocketFinalizer sock
       return sock

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
#endif

-----------------------------------------------------------------------------
-- Binding a socket

-- | Bind the socket to an address. The socket must not already be
-- bound.  The 'Family' passed to @bind@ must be the
-- same as that passed to 'socket'.  If the special port number
-- 'aNY_PORT' is passed then the system assigns the next available
-- use port.
bind :: Socket    -- Unconnected Socket
           -> SockAddr  -- Address to Bind to
           -> IO ()
bind (MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \ status -> do
  if status /= NotConnected
   then
    ioError (userError ("bind: can't peform bind on socket in status " ++
          show status))
   else do
    withSockAddr addr $ \p_addr sz -> do
     throwSocketErrorIfMinus1Retry_ "bind" $ c_bind s p_addr (fromIntegral sz)
     return Bound

-----------------------------------------------------------------------------
-- Connecting a socket

-- If the @PortID@ specifies a unix family socket and the @Hostname@
-- differs from that returned by @getHostname@ then an error is
-- raised. Alternatively an empty string may be given to @connectTo@
-- signalling that the current hostname applies.
data PortID =
          Service String                -- Service Name eg "ftp"
        | PortNumber PortNumber         -- User defined Port Number
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
        | UnixSocket String             -- Unix family socket in file system
#endif

-- | Convenience function.  Create a socket which is connected to the
-- given address.  The socket type is derived from the given port
-- identifier.  If a port number is given then the result is always an
-- internet family 'Stream' socket.
connectTo :: HostName           -- ^ Hostname
          -> PortID             -- ^ Port Identifier
          -> IO Socket          -- ^ Connected Socket
#if defined(IPV6_SOCKET_SUPPORT)
-- IPv6 and IPv4.
connectTo hostname (Service serv) = connect' hostname serv
connectTo hostname (PortNumber port) = connect' hostname (show port)
#else
-- IPv4 only.
connectTo hostname (Service serv) = do
    Exception.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close  -- only done if there's an error
        (\sock -> do
          port  <- getServicePortNumber serv
          he    <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
        )
connectTo hostname (PortNumber port) = do
    Exception.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close  -- only done if there's an error
        (\sock -> do
          he <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
        )
#endif
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
connectTo _ (UnixSocket path) = do
    Exception.bracketOnError
        (socket AF_UNIX Stream 0)
        close
        (\sock -> do
          connect sock (SockAddrUnix path)
          return sock
        )
#endif

#if defined(IPV6_SOCKET_SUPPORT)
connect' :: HostName -> ServiceName -> IO Socket
connect' host serv = do
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = defaultProtocol
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
    firstSuccessful $ map tryToConnect addrs
  where
  tryToConnect addr =
    Exception.bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close  -- only done if there's an error
        (\sock -> do
          connect sock (addrAddress addr)
          return sock
        )
#endif

-- | Connect to a remote socket at address.
connect :: Socket    -- Unconnected Socket
        -> SockAddr  -- Socket address stuff
        -> IO ()
connect sock@(MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \currentStatus -> do
  if currentStatus /= NotConnected && currentStatus /= Bound
   then
     ioError (userError ("connect: can't peform connect on socket in status " ++
         show currentStatus))
   else do
     withSockAddr addr $ \p_addr sz -> do

      let connectLoop = do
           r <- c_connect s p_addr (fromIntegral sz)
           if r == -1
               then do
#if !(defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS))
                   err <- getErrno
                   case () of
                     _ | err == eINTR       -> connectLoop
                     _ | err == eINPROGRESS -> connectBlocked
--                   _ | err == eAGAIN      -> connectBlocked
                     _                      -> throwSocketError "connect"
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
           threadWaitWrite (fromIntegral s)
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

-- | Creates the server side socket which has been bound to the
-- specified port.
--
-- NOTE: To avoid the \"Address already in use\"
-- problems popped up several times on the GHC-Users mailing list we
-- set the 'ReuseAddr' socket option on the listening socket.  If you
-- don't want this behaviour, please use the lower level
-- 'Network.Socket.listen' instead.

listenOn :: PortID         -- ^ Port Identifier
         -> IO Socket      -- ^ Connected Socket
#if defined(IPV6_SOCKET_SUPPORT)
-- IPv6 and IPv4.
listenOn (Service serv) = listen' serv
listenOn (PortNumber port) = listen' (show port)
#else
-- IPv4 only.
listenOn (Service serv) = do
    Exception.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close
        (\sock -> do
            port    <- getServicePortNumber serv
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrInet port iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )
listenOn (PortNumber port) = do
    Exception.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        close
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrInet port iNADDR_ANY)
            listen sock maxListenQueue
            return sock
        )
#endif
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
listenOn (UnixSocket path) =
    Exception.bracketOnError
        (socket AF_UNIX Stream 0)
        close
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (SockAddrUnix path)
            listen sock maxListenQueue
            return sock
        )
#endif

#if defined(IPV6_SOCKET_SUPPORT)
listen' :: ServiceName -> IO Socket
listen' serv = do
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
                             , addrSocketType = Stream
                             , addrProtocol = defaultProtocol }
    addrs <- getAddrInfo (Just hints) Nothing (Just serv)
    let addr = head addrs
    Exception.bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)
            listen sock maxListenQueue
            return sock
        )
#endif

-- | Listen for connections made to the socket.  The second argument
-- specifies the maximum number of queued connections and should be at
-- least 1; the maximum value is system-dependent (usually 5).
listen :: Socket  -- Connected & Bound Socket
       -> Int     -- Queue Length
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

-- | Accept a connection.  The socket must be bound to an address and
-- listening for connections.  The return value is a pair @(conn,
-- address)@ where @conn@ is a new socket object usable to send and
-- receive data on the connection, and @address@ is the address bound
-- to the socket on the other end of the connection.
accept :: Socket                        -- Queue Socket
       -> IO (Socket,                   -- Readable Socket
              SockAddr)                 -- Peer details

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
                 throwSocketErrorIfMinus1RetryMayBlock "accept"
                        (threadWaitRead (fromIntegral s))
                        (c_accept s sockaddr ptr_len)
# if __GLASGOW_HASKELL__ < 611
       System.Posix.Internals.setNonBlockingFD new_sock
# else
       System.Posix.Internals.setNonBlockingFD new_sock True
# endif
#endif
       addr <- peekSockAddr sockaddr
       newSock <- MkSocket new_sock family stype protocol `liftM`
                  newMVar Connected
       addSocketFinalizer newSock
       return (newSock, addr)

#if defined(mingw32_HOST_OS)
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
-- ** Sending and reciving data

-- ----------------------------------------------------------------------------
-- Sending

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Number of bytes sent
send (MkSocket s _ _ _ _) xs =
    B.unsafeUseAsCStringLen xs $ \(str, len) ->
    liftM fromIntegral $
#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)
#  if __GLASGOW_HASKELL__ >= 611
        writeRawBufferPtr "Network.Socket.ByteString.send"
        (FD s 1) (castPtr str) 0 (fromIntegral len)
#  else
        writeRawBufferPtr "Network.Socket.ByteString.send"
        (fromIntegral s) True str 0 (fromIntegral len)
#  endif
#else
        throwSocketErrorIfMinus1RetryMayBlock "send"
        (threadWaitWrite (fromIntegral s)) $
        c_send s str (fromIntegral len) 0
#endif

-- | Send data to the socket.  The socket must be connected to a
-- remote socket.  Unlike 'send', this function continues to send data
-- until either all data has been sent or an error occurs.  On error,
-- an exception is raised, and there is no way to determine how much
-- data, if any, was successfully sent.
sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for
-- ensuring that all data has been sent.
sendTo :: Socket      -- ^ Socket
       -> ByteString  -- ^ Data to send
       -> SockAddr    -- ^ Recipient address
       -> IO Int      -- ^ Number of bytes sent
sendTo sock xs addr =
    B.unsafeUseAsCStringLen xs $ \(str, len) -> sendBufTo sock str len addr

-- | Send data to the socket. The recipient can be specified
-- explicitly, so the socket need not be in a connected state.  Unlike
-- 'sendTo', this function continues to send data until either all
-- data has been sent or an error occurs.  On error, an exception is
-- raised, and there is no way to determine how much data, if any, was
-- successfully sent.
sendAllTo :: Socket      -- ^ Socket
          -> ByteString  -- ^ Data to send
          -> SockAddr    -- ^ Recipient address
          -> IO ()
sendAllTo sock xs addr = do
    sent <- sendTo sock xs addr
    when (sent < B.length xs) $ sendAllTo sock (B.drop sent xs) addr

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent.  Applications are responsible for
-- ensuring that all data has been sent.
sendBufTo :: Socket    -- ^ Socket
          -> Ptr a     -- ^ Pointer to data to send
          -> Int       -- ^ Number of bytes to send
          -> SockAddr  -- ^ Recipient address
          -> IO Int    -- ^ Number of bytes sent
sendBufTo (MkSocket s _family _stype _protocol _status) ptr nbytes addr = do
 withSockAddr addr $ \p_addr sz -> do
   liftM fromIntegral $
     throwSocketErrorIfMinus1RetryMayBlock "sendTo"
        (threadWaitWrite (fromIntegral s)) $
        c_sendto s ptr (fromIntegral $ nbytes) 0{-flags-}
                        p_addr (fromIntegral sz)

-- ----------------------------------------------------------------------------
-- ** Vectored I/O

-- $vectored
--
-- Vectored I\/O, also known as scatter\/gather I\/O, allows multiple
-- data segments to be sent using a single system call, without first
-- concatenating the segments.  For example, given a list of
-- @ByteString@s, @xs@,
--
-- > sendMany sock xs
--
-- is equivalent to
--
-- > sendAll sock (concat xs)
--
-- but potentially more efficient.
--
-- Vectored I\/O are often useful when implementing network protocols
-- that, for example, group data into segments consisting of one or
-- more fixed-length headers followed by a variable-length body.

-- | Send data to the socket.  The socket must be in a connected
-- state.  The data is sent as if the parts have been concatenated.
-- This function continues to send data until either all data has been
-- sent or an error occurs.  On error, an exception is raised, and
-- there is no way to determine how much data, if any, was
-- successfully sent.
sendMany :: Socket        -- ^ Connected socket
         -> [ByteString]  -- ^ Data to send
         -> IO ()
#if !defined(mingw32_HOST_OS)
sendMany sock@(MkSocket fd _ _ _ _) cs = do
    sent <- sendManyInner
    when (sent < totalLength cs) $ sendMany sock (remainingChunks sent cs)
  where
    sendManyInner =
      liftM fromIntegral . withIOVec cs $ \(iovsPtr, iovsLen) ->
          throwSocketErrorIfMinus1RetryMayBlock "writev"
              (threadWaitWrite (fromIntegral fd)) $
              c_writev (fromIntegral fd) iovsPtr (fromIntegral iovsLen)
#else
sendMany sock = sendAll sock . B.concat
#endif

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.  The
-- data is sent as if the parts have been concatenated.  This function
-- continues to send data until either all data has been sent or an
-- error occurs.  On error, an exception is raised, and there is no
-- way to determine how much data, if any, was successfully sent.
sendManyTo :: Socket        -- ^ Socket
           -> [ByteString]  -- ^ Data to send
           -> SockAddr      -- ^ Recipient address
           -> IO ()
#if !defined(mingw32_HOST_OS)
sendManyTo sock@(MkSocket fd _ _ _ _) cs addr = do
    sent <- liftM fromIntegral sendManyToInner
    when (sent < totalLength cs) $ sendManyTo sock (remainingChunks sent cs) addr
  where
    sendManyToInner =
      withSockAddr addr $ \addrPtr addrSize ->
        withIOVec cs $ \(iovsPtr, iovsLen) -> do
          let msgHdr = MsgHdr
                addrPtr (fromIntegral addrSize)
                iovsPtr (fromIntegral iovsLen)
          with msgHdr $ \msgHdrPtr ->
            throwSocketErrorIfMinus1RetryMayBlock "sendmsg"
              (threadWaitWrite (fromIntegral fd)) $
              c_sendmsg (fromIntegral fd) msgHdrPtr 0
#else
sendManyTo sock cs = sendAllTo sock (B.concat cs)
#endif

-- ----------------------------------------------------------------------------
-- Receiving

-- | Receive data from the socket.  The socket must be in a connected
-- state.  This function may return fewer bytes than specified.  If
-- the message is longer than the specified length, it may be
-- discarded depending on the type of socket.  This function may block
-- until a message arrives.
--
-- Considering hardware and network realities, the maximum number of bytes to
-- receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has
-- closed its half side of the connection.
recv :: Socket         -- ^ Connected socket
     -> Int            -- ^ Maximum number of bytes to receive
     -> IO ByteString  -- ^ Data received
recv (MkSocket s _ _ _ _) nbytes
    | nbytes < 0 = ioError (mkInvalidRecvArgError "Network.Socket.ByteString.recv")
    | otherwise  = B.createAndTrim nbytes $ recvInner s nbytes

recvInner :: CInt -> Int -> Ptr Word8 -> IO Int
recvInner s nbytes ptr =
    fmap fromIntegral $
#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)
#  if __GLASGOW_HASKELL__ >= 611
        readRawBufferPtr "Network.Socket.ByteString.recv" (FD s 1) ptr 0 (fromIntegral nbytes)
#  else
        readRawBufferPtr "Network.Socket.ByteString.recv" (fromIntegral s)
        True (castPtr ptr) 0 (fromIntegral nbytes)
#  endif
#else
        throwSocketErrorIfMinus1RetryMayBlock "recv"
        (threadWaitRead (fromIntegral s)) $
        c_recv s (castPtr ptr) (fromIntegral nbytes) 0
#endif

-- | Receive data from the socket, writing it into buffer instead of
-- creating a new string.  The socket need not be in a connected
-- state. Returns @(nbytes)@ where @nbytes@ is the number of
-- bytes received.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvBuf :: Socket -> Ptr a -> Int -> IO Int
recvBuf sock ptr nbytes = do
  (len, _) <- recvBufFrom' sock ptr nbytes
  return len

-- | Receive data from the socket.  The socket need not be in a
-- connected state.  Returns @(bytes, address)@ where @bytes@ is a
-- 'ByteString' representing the data received and @address@ is a
-- 'SockAddr' representing the address of the sending socket.
recvFrom :: Socket                     -- ^ Socket
         -> Int                        -- ^ Maximum number of bytes to receive
         -> IO (ByteString, SockAddr)  -- ^ Data received and sender address
recvFrom sock nbytes =
    allocaBytes nbytes $ \ptr -> do
        (len, sockaddr) <- recvBufFrom sock ptr nbytes
        str <- B.packCStringLen (ptr, len)
        return (str, sockaddr)

-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvBufFrom' :: Socket -> Ptr a -> Int -> IO (Int, Ptr SockAddr)
recvBufFrom' (MkSocket s family _stype _protocol _status) ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvFrom")
 | otherwise   =
    withNewSockAddr family $ \ptr_addr sz -> do
      alloca $ \ptr_len -> do
        poke ptr_len (fromIntegral sz)
        len <-
               throwSocketErrorIfMinus1RetryMayBlock "recvFrom"
                   (threadWaitRead (fromIntegral s)) $
                   c_recvfrom s ptr (fromIntegral nbytes) 0{-flags-}
                                ptr_addr ptr_len
        let len' = fromIntegral len
        if len' == 0
         then ioError (mkEOFError "Network.Socket.recvFrom")
         else return (len', ptr_addr)

-- | Receive data from the socket, writing it into buffer instead of
-- creating a new string.  The socket need not be in a connected
-- state. Returns @(nbytes, address)@ where @nbytes@ is the number of
-- bytes received and @address@ is a 'SockAddr' representing the
-- address of the sending socket.
--
-- NOTE: blocking on Windows unless you compile with -threaded (see
-- GHC ticket #1129)
recvBufFrom :: Socket -> Ptr a -> Int -> IO (Int, SockAddr)
recvBufFrom sock ptr nbytes = do
  (len, ptr_addr) <- recvBufFrom' sock ptr nbytes
  flg <- sIsConnected sock
    -- For at least one implementation (WinSock 2), recvfrom() ignores
    -- filling in the sockaddr for connected TCP sockets. Cope with
    -- this by using getPeerName instead.
  sockaddr <-
       if flg then
          getPeerName sock
       else
          peekSockAddr ptr_addr
  return (len, sockaddr)

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

socketPort :: Socket            -- Connected & Bound Socket
           -> IO PortNumber     -- Port Number of Socket
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

-- | Return the address of the peer connected to the specified socket.
-- This is useful to find out the port number of a remote IPv4/v6
-- socket, for instance.
getPeerName   :: Socket -> IO SockAddr
getPeerName (MkSocket s family _ _ _) =
    withNewSockAddr family $ \ptr sz ->
    with (fromIntegral sz) $ \int_star -> do
        throwSocketErrorIfMinus1Retry "getPeerName" $
            c_getpeername s ptr int_star
        peekSockAddr ptr

-- | Return the current address for the specified socket.  This is
-- useful to find out the port number of an IPv4/v6 socket, for
-- instance.
getSocketName :: Socket -> IO SockAddr
getSocketName (MkSocket s family _ _ _) =
    withNewSockAddr family $ \ptr sz ->
    with (fromIntegral sz) $ \int_star -> do
        throwSocketErrorIfMinus1Retry "getSocketName" $
            c_getsockname s ptr int_star
        peekSockAddr ptr

-----------------------------------------------------------------------------
-- Socket Properties

data SocketOption
    = DummySocketOption__  -- to make sure there's at least one constructor
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
    deriving Typeable

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
    Linger        -> #const SO_LINGER
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
                -> Int          -- Option Value
                -> IO ()
setSocketOption (MkSocket s _ _ _ _) so v = do
  with (fromIntegral v) $ \ptr_v -> do
   throwErrnoIfMinus1_ "setSocketOption" $
       c_setsockopt s (socketOptLevel so) (packSocketOption so) ptr_v
          (fromIntegral (sizeOf v))
   return ()


getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value
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
  let sz = #const sizeof(struct ucred)
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

##if !(MIN_VERSION_base(4,3,1))
closeFdWith :: (Fd -> IO ())  -- ^ Low-level action that performs the real close.
            -> Fd             -- ^ File descriptor to close.
            -> IO ()
closeFdWith closer fd = closer fd
##endif

#if defined(DOMAIN_SOCKET_SUPPORT)
-- sending/receiving ancillary socket data; low-level mechanism
-- for transmitting file descriptors, mainly.
sendFd :: Socket -> CInt -> IO ()
sendFd sock outfd = do
  let fd = fdSocket sock
  throwSocketErrorIfMinus1RetryMayBlock "sendFd"
     (threadWaitWrite (fromIntegral fd)) $
     c_sendFd fd outfd
  c_sendFd fd outfd
#endif
   -- Note: If Winsock supported FD-passing, thi would have been
   -- incorrect (since socket FDs need to be closed via closesocket().)
  closeFd outfd

recvFd :: Socket -> IO CInt
recvFd sock = do
  let fd = fdSocket sock
  theFd <-
    throwSocketErrorIfMinus1RetryMayBlock "recvFd"
        (threadWaitRead (fromIntegral fd)) $
         c_recvFd fd
  return theFd

foreign import ccall SAFE_ON_WIN "sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall SAFE_ON_WIN "recvFd" c_recvFd :: CInt -> IO CInt

-- ---------------------------------------------------------------------------
-- OS Dependent Definitions

packSocketType  :: SocketType -> CInt
unpackSocketType:: CInt -> SocketType

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
        deriving (Eq, Ord, Read, Show, Typeable)

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

foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32

#if defined(IPV6_SOCKET_SUPPORT)
-- | The IPv6 wild card address.

iN6ADDR_ANY :: HostAddress6
iN6ADDR_ANY = (0, 0, 0, 0)
#endif

-- | See 'maxListenQueue'.
sOMAXCONN :: Int
sOMAXCONN = #const SOMAXCONN

sOL_SOCKET :: Int
sOL_SOCKET = #const SOL_SOCKET

#ifdef SCM_RIGHTS
sCM_RIGHTS :: Int
sCM_RIGHTS = #const SCM_RIGHTS
#endif

-- | The maximum number of pending incoming connections to keep.  Also
-- see 'listen'.
maxListenQueue :: Int
maxListenQueue = sOMAXCONN

-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
firstSuccessful :: [IO a] -> IO a
firstSuccessful [] = error "firstSuccessful: empty list"
firstSuccessful (p:ps) = catchIO p $ \e ->
    case ps of
        [] -> Exception.throw e
        _  -> firstSuccessful ps

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
##if MIN_VERSION_base(4,0,0)
catchIO = Exception.catch
##else
catchIO = Exception.catchJust Exception.ioErrors
##endif

-- -----------------------------------------------------------------------------

-- | 'ShutdownCmd' is used to indicate which half of a connection
-- should be shut down.
data ShutdownCmd
    = ShutdownReceive  -- ^ Further receives will be disallowed
    | ShutdownSend     -- ^ Further sends will be disallowed
    | ShutdownBoth     -- ^ Further sends and receives will be disallowed
    deriving Typeable

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

-- | Shut down one or both halves of the connection, depending on the
-- second argument to the function.  If the second argument is
-- 'ShutdownReceive', further receives are disallowed.  If it is
-- 'ShutdownSend', further sends are disallowed.  If it is
-- 'ShutdownBoth', further sends and receives are disallowed.
shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry "shutdown" (c_shutdown s (sdownCmdToInt stype))
  return ()

-- -----------------------------------------------------------------------------

-- | Close the socket.  All future operations on the socket object
-- will fail.  The remote end will receive no more data (after queued
-- data is flushed).
close   :: Socket -> IO ()
close (MkSocket s _ _ _ socketStatus) = do
 modifyMVar_ socketStatus $ \ status ->
   case status of
     ConvertedToHandle ->
         ioError (userError ("close: converted to a Handle, use hClose instead"))
     Closed ->
         return status
     _ -> closeFdWith (closeFd . fromIntegral) (fromIntegral s) >> return Closed

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
sIsAcceptable (MkSocket _ AF_UNIX x _ status)
    | x == Stream || x == SeqPacket = do
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

-- | Turn a Socket into an 'Handle'. By default, the new handle is
-- unbuffered. Use 'System.IO.hSetBuffering' to change the buffering.
--
-- Note that since a 'Handle' is automatically closed by a finalizer
-- when it is no longer referenced, you should avoid doing any more
-- operations on the 'Socket' after calling 'toHandle'.  To
-- close the 'Socket' after 'toHandle', call 'System.IO.hClose'
-- on the 'Handle'.

#ifndef __PARALLEL_HASKELL__
toHandle :: Socket -> IOMode -> IO Handle
toHandle s@(MkSocket fd _ _ _ socketStatus) mode = do
 modifyMVar socketStatus $ \ status ->
    if status == ConvertedToHandle
        then ioError (userError ("toHandle: already a Handle"))
        else do
# if __GLASGOW_HASKELL__ >= 611
    h <- fdToHandle' (fromIntegral fd) (Just GHC.IO.Device.Stream) True (show s)
         mode True{-bin-}
# elif __GLASGOW_HASKELL__ >= 608
    h <- fdToHandle' (fromIntegral fd) (Just System.Posix.Internals.Stream) True
         (show s) mode True{-bin-}
# endif
    return (ConvertedToHandle, h)
#else
toHandle (MkSocket s family stype protocol status) m =
  error "toHandle not implemented in a parallel setup"
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
    deriving (Eq, Read, Show, Typeable)

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
    deriving (Eq, Show, Typeable)

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
    deriving (Eq, Read, Show, Typeable)

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
-- 'bind'.
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

-- ----------------------------------------------------------------------------
-- Not exported

#if !defined(mingw32_HOST_OS)
-- | Suppose we try to transmit a list of chunks @cs@ via a gathering write
-- operation and find that @n@ bytes were sent. Then @remainingChunks n cs@ is
-- list of chunks remaining to be sent.
remainingChunks :: Int -> [ByteString] -> [ByteString]
remainingChunks _ [] = []
remainingChunks i (x:xs)
    | i < len        = B.drop i x : xs
    | otherwise      = let i' = i - len in i' `seq` remainingChunks i' xs
  where
    len = B.length x

-- | @totalLength cs@ is the sum of the lengths of the chunks in the list @cs@.
totalLength :: [ByteString] -> Int
totalLength = sum . map B.length

-- | @withIOVec cs f@ executes the computation @f@, passing as argument a pair
-- consisting of a pointer to a temporarily allocated array of pointers to
-- 'IOVec' made from @cs@ and the number of pointers (@length cs@).
-- /Unix only/.
withIOVec :: [ByteString] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVec cs f =
    allocaArray csLen $ \aPtr -> do
        zipWithM_ pokeIov (ptrs aPtr) cs
        f (aPtr, csLen)
  where
    csLen = length cs
    ptrs = iterate (`plusPtr` sizeOf (undefined :: IOVec))
    pokeIov ptr s =
        B.unsafeUseAsCStringLen s $ \(sPtr, sLen) ->
        poke ptr $ IOVec sPtr (fromIntegral sLen)
#endif

-- ---------------------------------------------------------------------------
-- foreign imports from the C library

foreign import ccall unsafe "my_inet_ntoa"
  c_inet_ntoa :: HostAddress -> IO (Ptr CChar)

foreign import CALLCONV unsafe "inet_addr"
  c_inet_addr :: Ptr CChar -> IO HostAddress

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt

closeFd :: CInt -> IO ()
closeFd fd = throwErrnoIfMinus1Retry_ "Network.Socket.closeFd" $ c_close fd

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

#if defined(mingw32_HOST_OS) && defined(__GLASGOW_HASKELL__)
foreign import CALLCONV safe "accept"
  c_accept_safe :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
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
