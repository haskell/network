{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Test.Common
  ( serverAddr
  , testMsg
  , lazyTestMsg
  , tcpTest
  , tcpTestUsingClient
  , unixTest
  , unixTestWith
  , udpTest
  ) where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, readMVar)
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Network.Socket
import System.Directory
import qualified Data.ByteString.Lazy as L
import System.Timeout (timeout)

import Test.Hspec

serverAddr :: String
serverAddr = "127.0.0.1"

testMsg :: ByteString
testMsg = "This is a test message."

lazyTestMsg :: L.ByteString
lazyTestMsg = L.fromStrict "This is a test message."

unixAddr :: String
unixAddr = "/tmp/network-test"

-- | Establish a connection between client and server and then run
-- 'clientAct' and 'serverAct', in different threads.  Both actions
-- get passed a connected 'Socket', used for communicating between
-- client and server.  'unixTest' makes sure that the 'Socket' is
-- closed after the actions have run.
unixTest :: (Socket -> IO a) -> ((Socket, SockAddr) -> IO b) -> IO ()
unixTest = unixTestWith unixAddr unlink
  where
    unlink file = do
        exist <- doesFileExist file
        when exist $ removeFile file

unixTestWith
    :: String -- ^ address
    -> (String -> IO ()) -- ^ clean up action
    -> (Socket -> IO a) -- ^ client action
    -> ((Socket, SockAddr) -> IO b) -- ^ server action
    -> IO ()
unixTestWith address cleanupAct clientAct serverAct =
    test clientSetup clientAct serverSetup server
  where
    clientSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix address)
        return sock

    serverSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        cleanupAct address -- just in case
        bind sock (SockAddrUnix address)
        listen sock 1
        return sock

    server sock = E.bracket (accept sock) (killClientSock . fst) serverAct

    killClientSock sock = do
        shutdown sock ShutdownBoth
        close sock
        cleanupAct address

-- | Establish a connection between client and server and then run
-- 'clientAct' and 'serverAct', in different threads.  Both actions
-- get passed a connected 'Socket', used for communicating between
-- client and server.  'tcpTest' makes sure that the 'Socket' is
-- closed after the actions have run.
tcpTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
tcpTest clientAct serverAct =
    tcpTestUsingClient serverAct clientAct clientSetup
  where
    clientSetup portVar = do
        let hints = defaultHints { addrSocketType = Stream }
        serverPort <- readMVar portVar
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#if !defined(mingw32_HOST_OS)
        let fd = fdSocket sock
        getNonBlock fd `shouldReturn` True
        getCloseOnExec fd `shouldReturn` False
#endif
        connect sock $ addrAddress addr
        return sock

tcpTestUsingClient
    :: (Socket -> IO a) -> (Socket -> IO b) -> (MVar PortNumber -> IO Socket) -> IO ()
tcpTestUsingClient serverAct clientAct clientSetup = do
    portVar <- newEmptyMVar
    test (clientSetup portVar) clientAct (serverSetup portVar) server
  where
    serverSetup portVar = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        let fd = fdSocket sock
#if !defined(mingw32_HOST_OS)
        getNonBlock fd `shouldReturn` True
        getCloseOnExec fd `shouldReturn` False
#endif
        setSocketOption sock ReuseAddr 1
        setCloseOnExecIfNeeded fd
#if !defined(mingw32_HOST_OS)
        getCloseOnExec fd `shouldReturn` True
#endif
        bind sock $ addrAddress addr
        listen sock 1
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock

    server sock = do
        (clientSock, _) <- accept sock
#if !defined(mingw32_HOST_OS)
        let fd = fdSocket clientSock
        getNonBlock fd `shouldReturn` True
        getCloseOnExec fd `shouldReturn` True
#endif
        _ <- serverAct clientSock
        close clientSock

-- | Create an unconnected 'Socket' for sending UDP and receiving
-- datagrams and then run 'clientAct' and 'serverAct'.
udpTest :: (Socket -> PortNumber -> IO a) -> (Socket -> IO b) -> IO ()
udpTest clientAct serverAct = do
    portVar <- newEmptyMVar
    test clientSetup (client portVar) (serverSetup portVar) serverAct
  where
    clientSetup = socket AF_INET Datagram defaultProtocol

    client portVar sock = do
        serverPort <- readMVar portVar
        clientAct sock serverPort

    serverSetup portVar = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Datagram
              }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock $ addrAddress addr
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock

-- | Run a client/server pair and synchronize them so that the server
-- is started before the client and the specified server action is
-- finished before the client closes the 'Socket'.
test :: IO Socket -> (Socket -> IO b) -> IO Socket -> (Socket -> IO c) -> IO ()
test clientSetup clientAct serverSetup serverAct = do
    tid <- myThreadId
    barrier <- newEmptyMVar
    _ <- forkIO $ server barrier
        -- Release MVar if server setup fails
        `E.catch` \(e :: E.SomeException) -> putMVar barrier $ Just e
    client tid barrier
  where
    server barrier =
        E.bracket serverSetup close $ \sock -> do
        serverReady
        Just _ <- timeout 1000000 $ serverAct sock
        putMVar barrier Nothing
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier Nothing

    client tid barrier = do
        maybe (return ()) E.throwIO =<< takeMVar barrier
        -- Transfer exceptions to the main thread.
        bracketWithReraise tid clientSetup close $ \res -> do
            Just _ <- timeout 1000000 $ clientAct res
            maybe (return ()) E.throwIO =<< takeMVar barrier

-- | Like 'bracket' but catches and reraises the exception in another
-- thread, specified by the first argument.
bracketWithReraise :: ThreadId -> IO a -> (a -> IO b) -> (a -> IO ()) -> IO ()
bracketWithReraise tid setup teardown thing =
    E.bracket setup teardown thing
    `E.catch` \ (e :: E.SomeException) -> E.throwTo tid e
