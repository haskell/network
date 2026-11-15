{-# LANGUAGE ScopedTypeVariables #-}

-- | The Windows side of "Network.Socket.Unix".
module Network.Socket.Win32.Unix (
    socketPair,
) where

import qualified Control.Exception as E

import Network.Socket.Fcntl
import Network.Socket.Name (getSocketName)
import Network.Socket.Options
import Network.Socket.Syscall
import Network.Socket.Types

-- | A TCP connection over the loopback interface, whatever the arguments
--   are: an 'AF_UNIX' recv may miss the peer's close and wait for ever.
socketPair :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)
socketPair _ _ _ =
    E.bracket (socket AF_INET Stream defaultProtocol) close $ \listenSock -> do
        bind listenSock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen listenSock 10
        addr :: SockAddr <- getSocketName listenSock
        E.bracketOnError (socket AF_INET Stream defaultProtocol) close $ \clientSock -> do
            connect clientSock addr
            me :: SockAddr <- getSocketName clientSock
            serverSock <- acceptFrom listenSock me
            mapM_ prepare [clientSock, serverSock]
            return (clientSock, serverSock)
  where
    -- Another process may connect to the port first; it is not our peer.
    acceptFrom listenSock me = do
        (sock, peer) <- accept listenSock
        if peer == me
            then return sock
            else close sock >> acceptFrom listenSock me
    prepare sock = do
        setSocketOption sock NoDelay 1
        withFdSocket sock setNonBlockIfNeeded
