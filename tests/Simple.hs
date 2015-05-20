{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, readMVar)
import qualified Control.Exception as E
import Control.Monad (liftM, when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString

--- To tests for AF_CAN on Linux, you need to bring up a virtual (or real can
--- interface.). Run as root:
--- # modprobe can
--- # modprobe can_raw
--- # modprobe vcan
--- # sudo ip link add dev vcan0 type vcan
--- # ip link show vcan0
--- 3: can0: <NOARP,UP,LOWER_UP> mtu 16 qdisc noqueue state UNKNOWN link/can
--- Define HAVE_LINUX_CAN to run CAN tests as well.
--- #define HAVE_LINUX_CAN 1
-- #include "../include/HsNetworkConfig.h"
#if defined(HAVE_LINUX_CAN_H)
import Network.BSD (ifNameToIndex)
#endif
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

------------------------------------------------------------------------

serverAddr :: String
serverAddr = "127.0.0.1"

testMsg :: S.ByteString
testMsg = C.pack "This is a test message."

------------------------------------------------------------------------
-- Tests

------------------------------------------------------------------------
-- Sending and receiving

testSend :: Assertion
testSend = tcpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) testMsg
    client sock = send sock testMsg

testSendAll :: Assertion
testSendAll = tcpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) testMsg
    client sock = sendAll sock testMsg

testSendTo :: Assertion
testSendTo = udpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) testMsg
    client sock serverPort = do
        addr <- inet_addr serverAddr
        sendTo sock testMsg (SockAddrInet serverPort addr)

testSendAllTo :: Assertion
testSendAllTo = udpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) testMsg
    client sock serverPort = do
        addr <- inet_addr serverAddr
        sendAllTo sock testMsg (SockAddrInet serverPort addr)

testSendMany :: Assertion
testSendMany = tcpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) (S.append seg1 seg2)
    client sock = sendMany sock [seg1, seg2]

    seg1 = C.pack "This is a "
    seg2 = C.pack "test message."

testSendManyTo :: Assertion
testSendManyTo = udpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) (S.append seg1 seg2)
    client sock serverPort = do
        addr <- inet_addr serverAddr
        sendManyTo sock [seg1, seg2] (SockAddrInet serverPort addr)

    seg1 = C.pack "This is a "
    seg2 = C.pack "test message."

testRecv :: Assertion
testRecv = tcpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) testMsg
    client sock = send sock testMsg

testOverFlowRecv :: Assertion
testOverFlowRecv = tcpTest client server
  where
    server sock = do seg1 <- recv sock (S.length testMsg - 3)
                     seg2 <- recv sock 1024
                     let msg = S.append seg1 seg2
                     testMsg @=? msg

    client sock = send sock testMsg

testRecvFrom :: Assertion
testRecvFrom = tcpTest client server
  where
    server sock = do (msg, _) <- recvFrom sock 1024
                     testMsg @=? msg

    client sock = do
        serverPort <- getPeerPort sock
        addr <- inet_addr serverAddr
        sendTo sock testMsg (SockAddrInet serverPort addr)

testOverFlowRecvFrom :: Assertion
testOverFlowRecvFrom = tcpTest client server
  where
    server sock = do (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                     (seg2, _) <- recvFrom sock 1024
                     let msg = S.append seg1 seg2
                     testMsg @=? msg

    client sock = send sock testMsg

testUserTimeout :: Assertion
testUserTimeout = do
    when (isSupportedSocketOption UserTimeout) $ do
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock UserTimeout 1000
      getSocketOption sock UserTimeout >>= (@=?) 1000
      setSocketOption sock UserTimeout 2000
      getSocketOption sock UserTimeout >>= (@=?) 2000
      sClose sock

{-
testGetPeerCred:: Assertion
testGetPeerCred =
    test clientSetup clientAct serverSetup server
  where
    clientSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock $ SockAddrUnix addr 
        return sock

    serverSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        bindSocket sock $ SockAddrUnix addr 
        listen sock 1
        return sock

    server sock = do
        (clientSock, _) <- accept sock
        serverAct clientSock
        sClose clientSock

    addr = "/tmp/testAddr1"
    clientAct sock = withSocketsDo $ do  
                     sendAll sock testMsg
                     (pid,uid,gid) <- getPeerCred sock
                     putStrLn $ unwords ["pid=",show pid,"uid=",show uid, "gid=", show gid]
    serverAct sock = withSocketsDo $ do
                     msg <- recv sock 1024
                     putStrLn $ C.unpack msg


testGetPeerEid :: Assertion
testGetPeerEid =  
    test clientSetup clientAct serverSetup server
  where
    clientSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock $ SockAddrUnix addr 
        return sock

    serverSetup = do
        sock <- socket AF_UNIX Stream defaultProtocol
        bindSocket sock $ SockAddrUnix addr 
        listen sock 1
        return sock

    server sock = do
        (clientSock, _) <- accept sock
        serverAct clientSock
        sClose clientSock

    addr = "/tmp/testAddr2"
    clientAct sock = withSocketsDo $ do  
                     sendAll sock testMsg
                     (uid,gid) <- getPeerEid sock
                     putStrLn $ unwords ["uid=",show uid, "gid=", show gid]
    serverAct sock = withSocketsDo $ do
                     msg <- recv sock 1024
                     putStrLn $ C.unpack msg
-}

#if defined(HAVE_LINUX_CAN_H)
canTestMsg = S.pack [ 0,0,0,0 -- can ID = 0
                    , 4,0,0,0 -- data length counter = 2 (bytes)
                    , 0x80,123,321,55 -- SYNC with some random extra bytes
                    , 0, 0, 0, 0 -- padding
                    ]

testCanSend :: Assertion
testCanSend = canTest "vcan0" client server
  where
    server sock = recv sock 1024 >>= (@=?) canTestMsg
    client sock = send sock canTestMsg

canTest :: String -> (Socket -> IO a) -> (Socket -> IO b) -> IO ()
canTest ifname clientAct serverAct = do
    ifIndex <- liftM fromJust $ ifNameToIndex ifname
    test (clientSetup ifIndex) clientAct (serverSetup ifIndex) serverAct
  where
    clientSetup ifIndex = do
      sock <- socket AF_CAN Raw 1 -- protocol 1 = raw CAN
      -- bind the socket to the interface
      bind sock (SockAddrCan $ fromIntegral $ ifIndex)
      return sock
    
    serverSetup = clientSetup
#endif

------------------------------------------------------------------------
-- Other

------------------------------------------------------------------------
-- List of all tests

basicTests :: Test
basicTests = testGroup "Basic socket operations"
    [
      -- Sending and receiving
      testCase "testSend" testSend
    , testCase "testSendAll" testSendAll
    , testCase "testSendTo" testSendTo
    , testCase "testSendAllTo" testSendAllTo
    , testCase "testSendMany" testSendMany
    , testCase "testSendManyTo" testSendManyTo
    , testCase "testRecv" testRecv
    , testCase "testOverFlowRecv" testOverFlowRecv
    , testCase "testRecvFrom" testRecvFrom
    , testCase "testOverFlowRecvFrom" testOverFlowRecvFrom
    , testCase "testUserTimeout" testUserTimeout
--    , testCase "testGetPeerCred" testGetPeerCred
--    , testCase "testGetPeerEid" testGetPeerEid
#if defined(HAVE_LINUX_CAN_H)
    , testCase "testCanSend" testCanSend  
#endif
    ]

tests :: [Test]
tests = [basicTests]

------------------------------------------------------------------------
-- Test helpers

-- | Returns the 'PortNumber' of the peer. Will throw an 'error' if
-- used on a non-IP socket.
getPeerPort :: Socket -> IO PortNumber
getPeerPort sock = do
    sockAddr <- getPeerName sock
    case sockAddr of
        (SockAddrInet port _) -> return port
        (SockAddrInet6 port _ _ _) -> return port
        _ -> error "getPeerPort: only works with IP sockets"

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
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr serverAddr
        serverPort <- readMVar portVar
        connect sock $ SockAddrInet serverPort addr
        return sock

    serverSetup portVar = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        addr <- inet_addr serverAddr
        bindSocket sock (SockAddrInet aNY_PORT addr)
        listen sock 1
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock

    server sock = do
        (clientSock, _) <- accept sock
        serverAct clientSock
        sClose clientSock

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
        sock <- socket AF_INET Datagram defaultProtocol
        setSocketOption sock ReuseAddr 1
        addr <- inet_addr serverAddr
        bindSocket sock (SockAddrInet aNY_PORT addr)
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
    forkIO $ server barrier
    client tid barrier
  where
    server barrier = do
        E.bracket serverSetup sClose $ \sock -> do
            serverReady
            serverAct sock
            putMVar barrier ()
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier ()

    client tid barrier = do
        takeMVar barrier
        -- Transfer exceptions to the main thread.
        bracketWithReraise tid clientSetup sClose $ \res -> do
            clientAct res
            takeMVar barrier

-- | Like 'bracket' but catches and reraises the exception in another
-- thread, specified by the first argument.
bracketWithReraise :: ThreadId -> IO a -> (a -> IO b) -> (a -> IO ()) -> IO ()
bracketWithReraise tid before after thing =
    E.bracket before after thing
    `E.catch` \ (e :: E.SomeException) -> E.throwTo tid e

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = withSocketsDo $ defaultMain tests
