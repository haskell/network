{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

------------------------------------------------------------------------

serverPort :: PortNumber
serverPort = fromIntegral (3000 :: Int)

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
      client sock = do addr <- inet_addr serverAddr
                       sendTo sock testMsg (SockAddrInet serverPort addr)

testSendAllTo :: Assertion
testSendAllTo = udpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) testMsg
      client sock = do addr <- inet_addr serverAddr
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
      client sock = do addr <- inet_addr serverAddr
                       sendManyTo sock [seg1, seg2]
                           (SockAddrInet serverPort addr)

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

      client sock = do addr <- inet_addr serverAddr
                       sendTo sock testMsg (SockAddrInet serverPort addr)

testOverFlowRecvFrom :: Assertion
testOverFlowRecvFrom = tcpTest client server
    where
      server sock = do (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                       (seg2, _) <- recvFrom sock 1024
                       let msg = S.append seg1 seg2
                       testMsg @=? msg

      client sock = send sock testMsg

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
    ]

tests :: [Test]
tests = [basicTests]

------------------------------------------------------------------------
-- Test helpers

-- | Establish a connection between client and server and then run
-- 'clientAct' and 'serverAct', in different threads.  Both actions
-- get passed a connected 'Socket', used for communicating between
-- client and server.  'tcpTest' makes sure that the 'Socket' is
-- closed after the actions have run.
tcpTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
tcpTest clientAct serverAct =
    test clientSetup clientAct serverSetup server
  where
    clientSetup = do
        sock <- socket AF_INET Stream defaultProtocol
        addr <- inet_addr serverAddr
        connect sock $ SockAddrInet serverPort addr
        return sock

    serverSetup = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        addr <- inet_addr serverAddr
        bindSocket sock (SockAddrInet serverPort addr)
        listen sock 1
        return sock

    server sock = do
        (clientSock, _) <- accept sock
        serverAct clientSock
        sClose clientSock

-- | Create an unconnected 'Socket' for sending UDP and receiving
-- datagrams and then run 'clientAct' and 'serverAct'.
udpTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
udpTest clientAct serverAct =
    test clientSetup clientAct serverSetup serverAct
  where
    clientSetup = socket AF_INET Datagram defaultProtocol

    serverSetup = do
        sock <- socket AF_INET Datagram defaultProtocol
        setSocketOption sock ReuseAddr 1
        addr <- inet_addr serverAddr
        bindSocket sock (SockAddrInet serverPort addr)
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
