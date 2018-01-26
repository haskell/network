{-# LANGUAGE CPP, ScopedTypeVariables #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, readMVar)
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString
import System.Timeout (timeout)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?), assertBool, assertFailure)

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
        let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
        sendTo sock testMsg $ addrAddress addr

testSendAllTo :: Assertion
testSendAllTo = udpTest client server
  where
    server sock = recv sock 1024 >>= (@=?) testMsg
    client sock serverPort = do
        let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
        sendAllTo sock testMsg $ addrAddress addr

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
        let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
        sendManyTo sock [seg1, seg2] $ addrAddress addr

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
        addr <- getPeerName sock
        sendTo sock testMsg addr

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
      close sock

testGetPeerCredential :: Assertion
testGetPeerCredential = when isUnixDomainSocketAvailable $ do
    s <- socket AF_INET Stream defaultProtocol
    cred1 <- getPeerCredential s
    (Nothing,Nothing,Nothing) @=? cred1
    (x,_) <- socketPair AF_UNIX Stream defaultProtocol
    (_,muid,_) <- getPeerCredential x
    assertBool "testGetPeerCredential" (muid /= Nothing)

testByteStringEol :: Assertion
testByteStringEol = tcpTest client (flip shutdown ShutdownSend)
  where client s = do
          res :: Either IOError C.ByteString <- E.try $ recv s 4096
          res @=? Right S.empty

------------------------------------------------------------------------
-- Conversions of IP addresses

testHostAddressToTuple :: Assertion
testHostAddressToTuple = do
    -- Look up a numeric IPv4 host
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
    (AddrInfo{addrAddress = (SockAddrInet _ hostAddr)} : _) <-
        getAddrInfo (Just hints) (Just "127.128.129.130") Nothing
    -- and check that the decoded address matches the expected representation
    (0x7f, 0x80, 0x81, 0x82) @=? hostAddressToTuple hostAddr

testHostAddressToTupleInv :: Assertion
testHostAddressToTupleInv = do
    let addr = (0x7f, 0x80, 0x81, 0x82)
    addr @=? (hostAddressToTuple . tupleToHostAddress) addr

#if defined(IPV6_SOCKET_SUPPORT)
testHostAddress6ToTuple :: Assertion
testHostAddress6ToTuple = do
    -- Look up a numeric IPv6 host
    let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
        host = "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
    (AddrInfo{addrAddress = (SockAddrInet6 _ _ hostAddr _)} : _) <-
        getAddrInfo (Just hints) (Just host) Nothing
    -- and check that the decoded address matches the expected representation
    (0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334)
        @=? hostAddress6ToTuple hostAddr

testHostAddress6ToTupleInv :: Assertion
testHostAddress6ToTupleInv = do
    let addr = (0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334)
    addr @=? (hostAddress6ToTuple . tupleToHostAddress6) addr
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
    , testCase "testGetPeerCredential" testGetPeerCredential
    , testCase "testByteStringEol" testByteStringEol
      -- conversions of IP addresses
    , testCase "testHostAddressToTuple" testHostAddressToTuple
    , testCase "testHostAddressToTupleInv" testHostAddressToTupleInv
#if defined(IPV6_SOCKET_SUPPORT)
    , testCase "testHostAddress6ToTuple" testHostAddress6ToTuple
    , testCase "testHostAddress6ToTupleInv" testHostAddress6ToTupleInv
#endif
      -- other
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
        _ -> ioError $ userError "getPeerPort: only works with IP sockets"

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
#if !defined(mingw32_HOST_OS)
        getNonBlock (fdSocket sock) >>= (@=?) True
        getCloseOnExec (fdSocket sock) >>= (@=?) False
#endif
        connect sock $ addrAddress addr
        return sock

    serverSetup portVar = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#if !defined(mingw32_HOST_OS)
        getNonBlock (fdSocket sock) >>= (@=?) True
        getCloseOnExec (fdSocket sock) >>= (@=?) False
#endif
        setSocketOption sock ReuseAddr 1
        setCloseOnExecIfNeeded (fdSocket sock)
#if !defined(mingw32_HOST_OS)
        getCloseOnExec (fdSocket sock) >>= (@=?) True
#endif
        bind sock $ addrAddress addr
        listen sock 1
        serverPort <- socketPort sock
        putMVar portVar serverPort
        return sock

    server sock = do
        (clientSock, _) <- accept sock
#if !defined(mingw32_HOST_OS)
        getNonBlock (fdSocket clientSock) >>= (@=?) True
        getCloseOnExec (fdSocket clientSock) >>= (@=?) True
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
bracketWithReraise tid before after thing =
    E.bracket before after thing
    `E.catch` \ (e :: E.SomeException) -> E.throwTo tid e

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = withSocketsDo $ defaultMain tests
