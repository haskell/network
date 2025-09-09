{-# LANGUAGE CPP #-}
module Network.Socket.Syscall.WinIO where

#include "HsNet.h"
##include "HsNetDef.h"

import Control.Concurrent.STM
import qualified Control.Exception as E
import Foreign.Marshal.Alloc (allocaBytes, alloca)
import Foreign.Marshal.Utils (with)
import GHC.Event.Windows
import GHC.Windows
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Options
import Network.Socket.Types (
    Family(..),
    SocketAddress,
    SocketType(..),
    ProtocolNumber,
    withSocketAddress,
    peekSocketAddress
    )
import qualified Network.Socket.Types as Generic
import Network.Socket.Types.WinIO
import Network.Socket.Win32.Load
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)

socket :: Family         -- Family Name (usually AF_INET)
    -> SocketType     -- Socket Type (usually Stream)
    -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
    -> IO Socket      -- Unconnected Socket
socket family stype protocol = E.bracketOnError create c_close $ \s -> do
    associateHandle' s
    sock <- mkSocket s
    unsetIPv6Only sock
    setDontFragment sock
    pure sock
  where
    create = do
        s <- c_socket (packFamily family) (packSocketType stype) protocol
        when (s == invalidSocket) $ throwSocketError "Network.Socket.socket"
        pure s
#if HAVE_DECL_IPV6_V6ONLY
    unsetIPv6Only s = when (family == AF_INET6 && stype `elem` [Stream, Datagram]) $
      -- The IPv6Only option is only supported on Windows Vista and later,
      -- so trying to change it might throw an error.
      setSocketOption (Generic.Socket $ Right s) IPv6Only 0 `catchIOError` \_ -> return ()
#else
    unsetIPv6Only _ = return ()
#endif
    setDontFragment s = when (family == AF_INET) $
#if HAVE_DECL_IP_DONTFRAG || HAVE_DECL_IP_MTU_DISCOVER
      setSocketOption (Generic.Socket $ Right s) DontFragment 1
#else
      -- do nothing
      return ()
#endif

foreign import CALLCONV unsafe "socket"
    c_socket :: CInt -> CInt -> CInt -> IO SOCKET

bind :: SocketAddress sa => Socket -> sa -> IO ()
bind s sa = withSocketAddress sa $ \p_sa siz -> withSOCKET s $ \sock -> do
    let sz = fromIntegral siz
    void $ throwSocketErrorIfMinus1Retry "Network.Socket.bind" $ c_bind sock p_sa sz

foreign import CALLCONV unsafe "bind"
    c_bind :: SOCKET -> Ptr sa -> CInt{-CSockLen???-} -> IO CInt

connect :: Socket -> Generic.SockAddr -> IO ()
connect s saddr = do
    -- NB: ConnectEx assumes the socket is already bound
    bindTo <- case saddr of
        Generic.SockAddrInet _ _ -> pure $ Generic.SockAddrInet 0 0
        Generic.SockAddrInet6 _ _ _ _ -> pure $ Generic.SockAddrInet6 0 0 (0, 0, 0, 0) 0
        Generic.SockAddrUnix _ -> throwSocketError "Network.Socket.connect: Unix socket on Windows"
    bind s bindTo
    withSOCKET s $ \sock -> do
        cex <- loadConnectEx sock
        withSocketAddress saddr $ \p_sa sz -> do
            withOverlapped "Network.Socket.connect: ConnectEx" sock 0
                (\lpOverlapped -> do
                    ret <- cex sock p_sa (fromIntegral sz) nullPtr 0 nullPtr lpOverlapped
                    if ret
                        then pure CbPending
                        else do
                        le <- c_getLastError
                        pure $ if le == #{const ERROR_IO_PENDING}
                            then CbPending
                            else CbError (fromIntegral le)
                )
                (\errCode _ -> case errCode of
                    0 -> ioSuccess 0
                    -- See below
                    _ -> ioFailed $ ntStatusToDosError errCode
                )

            -- Make the connected socket fully functional.
            -- Required for shutdown/getpeername/getsockname to work.
            void $ throwSocketErrorIfMinus1Retry
                "Network.Socket.connect: setsockopt(SO_UPDATE_CONNECT_CONTEXT)" $
                c_setsockopt sock #{const SOL_SOCKET} #{const SO_UPDATE_CONNECT_CONTEXT}
                    nullPtr 0

-- | Convert an NTSTATUS code to a Win32 error code.
-- The GHC IOCP manager delivers NTSTATUS values in completion callbacks;
-- Winsock functions expect Win32 error codes.
foreign import ccall unsafe "RtlNtStatusToDosError"
    ntStatusToDosError :: ErrCode -> ErrCode

connectExPtr :: TVar (Loaded a)
connectExPtr = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE connectExPtr #-}

loadConnectEx :: SOCKET -> IO (ConnectEx sa)
loadConnectEx = loadExtensionFunction connectExPtr c_loadConnectEx mkConnectEx "ConnectEx"

type ConnectEx sa =
    SOCKET              -- s
    -> Ptr sa           -- name
    -> CInt             -- namelen
    -> Ptr ()           -- lpSendBuffer (nullable)
    -> DWORD            -- dwSendDataLength
    -> Ptr DWORD        -- lpdwBytesSent
    -> LPOVERLAPPED   -- lpOverlapped
    -> IO BOOL

foreign import ccall unsafe "dynamic"
    mkConnectEx :: FunPtr (ConnectEx sa) -> ConnectEx sa

foreign import ccall unsafe "loadConnectEx"
    c_loadConnectEx :: SOCKET -> IO (FunPtr (ConnectEx sa))


listen :: Socket -> Int -> IO ()
listen s backlog = withSOCKET s $ \sock ->
    throwSocketErrorIfMinus1Retry_ "Network.Socket.listen" $
        c_listen sock (fromIntegral backlog)

foreign import CALLCONV unsafe "listen"
    c_listen :: SOCKET -> CInt -> IO CInt


accept :: SocketAddress sa => Socket -> IO (Socket, sa)
accept listing_sock = withSOCKET listing_sock $ \lsock -> do
    aex <- loadAcceptEx lsock
    gasas <- loadGetAcceptExSockaddrs lsock

    -- AcceptEx buffer: no receive data, just addresses.
    -- Each address slot must be >= sizeof(sockaddr_storage) + 16.
    let addrLen = #{size struct sockaddr_storage} + 16 :: DWORD
        bufSize = fromIntegral (2 * addrLen) :: Int

    -- AcceptEx requires a pre-created, unbound accept socket.
    asock <- c_createPeerSocket lsock
    when (asock == invalidSocket) $
        throwSocketError "Network.Socket.accept: can't create peer socket"

    E.bracketOnError (pure asock) c_close $ \_ ->
      allocaBytes bufSize $ \buf -> do
        -- Overlapped AcceptEx on the LISTENING socket's IOCP handle
        void $ withOverlapped "Network.Socket.accept" lsock 0
            (\lpOverlapped -> do
                ret <- aex lsock asock buf 0 addrLen addrLen nullPtr lpOverlapped
                if ret
                    then pure CbPending  -- AcceptEx TRUE still queues IOCP completion
                    else do
                        le <- c_getLastError
                        pure $ if le == #{const ERROR_IO_PENDING}
                            then CbPending
                            else CbError (fromIntegral le)
            )
            (\errCode _ -> case errCode of
                0 -> ioSuccess 0
                _ -> ioFailed $ ntStatusToDosError errCode
            )

        -- Make accepted socket inherit listen socket properties.
        -- Required for getpeername/getsockname to work.
        with lsock $ \lsockPtr ->
            void $ throwSocketErrorIfMinus1Retry
                "Network.Socket.accept" $
                c_setsockopt asock #{const SOL_SOCKET} #{const SO_UPDATE_ACCEPT_CONTEXT}
                    (castPtr lsockPtr) (fromIntegral $ sizeOf lsock)

        -- Extract remote address from AcceptEx output buffer
        alloca $ \localPtrPtr ->
          alloca $ \localLenPtr ->
            alloca $ \remotePtrPtr ->
              alloca $ \remoteLenPtr -> do
                gasas buf 0 addrLen addrLen
                    localPtrPtr localLenPtr remotePtrPtr remoteLenPtr
                remotePtr <- peek remotePtrPtr
                remoteAddr <- peekSocketAddress (castPtr remotePtr)

                -- Register with IOCP and create Socket wrapper
                associateHandle' asock
                sock <- mkSocket asock
                return (sock, remoteAddr)

-- AcceptEx function pointer type
type AcceptExFn =
    SOCKET       -- sListenSocket
    -> SOCKET    -- sAcceptSocket
    -> Ptr ()    -- lpOutputBuffer
    -> DWORD     -- dwReceiveDataLength
    -> DWORD     -- dwLocalAddressLength
    -> DWORD     -- dwRemoteAddressLength
    -> Ptr DWORD -- lpdwBytesReceived
    -> LPOVERLAPPED
    -> IO BOOL

-- GetAcceptExSockaddrs function pointer type
type GetAcceptExSockaddrsFn =
    Ptr ()       -- lpOutputBuffer
    -> DWORD     -- dwReceiveDataLength
    -> DWORD     -- dwLocalAddressLength
    -> DWORD     -- dwRemoteAddressLength
    -> Ptr (Ptr ()) -- LocalSockaddr (output)
    -> Ptr CInt     -- LocalSockaddrLength (output)
    -> Ptr (Ptr ()) -- RemoteSockaddr (output)
    -> Ptr CInt     -- RemoteSockaddrLength (output)
    -> IO ()

foreign import ccall unsafe "dynamic"
    mkAcceptEx :: FunPtr AcceptExFn -> AcceptExFn

foreign import ccall unsafe "dynamic"
    mkGetAcceptExSockaddrs :: FunPtr GetAcceptExSockaddrsFn -> GetAcceptExSockaddrsFn

foreign import ccall unsafe "loadAcceptEx"
    c_loadAcceptEx :: SOCKET -> IO (FunPtr AcceptExFn)

foreign import ccall unsafe "loadGetAcceptExSockaddrs"
    c_loadGetAcceptExSockaddrs :: SOCKET -> IO (FunPtr GetAcceptExSockaddrsFn)

foreign import ccall unsafe "createPeerSocket"
    c_createPeerSocket :: SOCKET -> IO SOCKET

foreign import CALLCONV unsafe "setsockopt"
    c_setsockopt :: SOCKET -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

acceptExPtr :: TVar (Loaded a)
acceptExPtr = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE acceptExPtr #-}

loadAcceptEx :: SOCKET -> IO AcceptExFn
loadAcceptEx = loadExtensionFunction acceptExPtr c_loadAcceptEx mkAcceptEx "AcceptEx"

getAcceptExSockaddrsPtr :: TVar (Loaded a)
getAcceptExSockaddrsPtr = unsafePerformIO $ newTVarIO Unloaded
{-# NOINLINE getAcceptExSockaddrsPtr #-}

loadGetAcceptExSockaddrs :: SOCKET -> IO GetAcceptExSockaddrsFn
loadGetAcceptExSockaddrs = loadExtensionFunction getAcceptExSockaddrsPtr c_loadGetAcceptExSockaddrs mkGetAcceptExSockaddrs "GetAcceptExSockaddrs"
