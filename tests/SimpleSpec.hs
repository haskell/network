{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimpleSpec (main, spec) where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, readMVar)
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Timeout (timeout)

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "send" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = send sock testMsg
            tcpTest client server

    describe "sendAll" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = sendAll sock testMsg
            tcpTest client server

    describe "sendTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock serverPort = do
                    let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
                    addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
                    sendTo sock testMsg $ addrAddress addr
            udpTest client server

    describe "sendAllTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock serverPort = do
                    let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
                    addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
                    sendAllTo sock testMsg $ addrAddress addr
            udpTest client server

    describe "sendMany" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` (S.append seg1 seg2)
                client sock = sendMany sock [seg1, seg2]

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            tcpTest client server

    describe "sendManyTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` (S.append seg1 seg2)
                client sock serverPort = do
                    let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
                    addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
                    sendManyTo sock [seg1, seg2] $ addrAddress addr

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            udpTest client server

    describe "recv" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do seg1 <- recv sock (S.length testMsg - 3)
                                 seg2 <- recv sock 1024
                                 let msg = S.append seg1 seg2
                                 msg `shouldBe` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "returns empty string at EOF" $ do
            let client s = recv s 4096 `shouldReturn` S.empty
                server s = shutdown s ShutdownSend
            tcpTest client server

    describe "recvFrom" $ do
        it "works well" $ do
            let server sock = do (msg, _) <- recvFrom sock 1024
                                 testMsg `shouldBe` msg
                client sock = do
                    addr <- getPeerName sock
                    sendTo sock testMsg addr
            tcpTest client server
        it "can treat overflow" $ do
            let server sock = do (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                                 (seg2, _) <- recvFrom sock 1024
                                 let msg = S.append seg1 seg2
                                 testMsg `shouldBe` msg

                client sock = send sock testMsg
            tcpTest client server

    describe "UserTimeout" $ do
        it "can be set" $ do
            when (isSupportedSocketOption UserTimeout) $ do
              sock <- socket AF_INET Stream defaultProtocol
              setSocketOption sock UserTimeout 1000
              getSocketOption sock UserTimeout `shouldReturn` 1000
              setSocketOption sock UserTimeout 2000
              getSocketOption sock UserTimeout `shouldReturn` 2000
              close sock

    describe "getAddrInfo" $ do
        it "works for IPv4 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
            AddrInfo{addrAddress = (SockAddrInet _ hostAddr)}:_ <-
                getAddrInfo (Just hints) (Just "127.128.129.130") Nothing
            hostAddressToTuple hostAddr `shouldBe` (0x7f, 0x80, 0x81, 0x82)
#if defined(IPV6_SOCKET_SUPPORT)
        it "works for IPv6 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
                host = "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
            AddrInfo{addrAddress = (SockAddrInet6 _ _ hostAddr _)}:_ <-
                getAddrInfo (Just hints) (Just host) Nothing
            hostAddress6ToTuple hostAddr
                `shouldBe` (0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334)
#endif

------------------------------------------------------------------------

serverAddr :: String
serverAddr = "127.0.0.1"

testMsg :: ByteString
testMsg = "This is a test message."

------------------------------------------------------------------------
-- Test helpers

-- | Establish a connection between client and server and then run
-- 'clientAct' and 'serverAct', in different threads.  Both actions
-- get passed a connected 'Socket', used for communicating between
-- client and server.  'tcpTest' makes sure that the 'Socket' is
-- closed after the actions have run.
tcpTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
tcpTest clientAct serverAct = do
    portVar <- newEmptyMVar
    test (clientSetup portVar) clientAct (serverSetup portVar) server
  where
    clientSetup portVar = do
        let hints = defaultHints { addrSocketType = Stream }
        serverPort <- readMVar portVar
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

    serverSetup portVar = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock $ addrAddress addr
        listen sock 1
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock

    server sock = do
        (clientSock, _) <- accept sock
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
    client tid barrier
  where
    server barrier = do
        E.bracket serverSetup close $ \sock -> do
            serverReady
            Just _ <- timeout 1000000 $ serverAct sock
            putMVar barrier ()
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier ()

    client tid barrier = do
        takeMVar barrier
        -- Transfer exceptions to the main thread.
        bracketWithReraise tid clientSetup close $ \res -> do
            Just _ <- timeout 1000000 $ clientAct res
            takeMVar barrier

-- | Like 'bracket' but catches and reraises the exception in another
-- thread, specified by the first argument.
bracketWithReraise :: ThreadId -> IO a -> (a -> IO b) -> (a -> IO ()) -> IO ()
bracketWithReraise tid setup teardown thing =
    E.bracket setup teardown thing
    `E.catch` \ (e :: E.SomeException) -> E.throwTo tid e
