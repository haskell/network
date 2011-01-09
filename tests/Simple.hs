{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, catch, throwTo)
import Control.Monad (when)
import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Prelude hiding (catch)
import System.Exit (exitFailure)
import Test.HUnit (Counts(..), Test(..), (@=?), runTestTT)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L

import Network.Socket.ByteString (recv, recvFrom, send, send, sendAll,
                                  sendTo, sendAllTo, sendMany, sendManyTo)
import qualified Network.Socket.ByteString.Lazy as NSBL

------------------------------------------------------------------------

serverPort :: PortNumber
serverPort = fromIntegral (3000 :: Int)

serverAddr :: String
serverAddr = "127.0.0.1"

testMsg :: S.ByteString
testMsg = C.pack "This is a test message."

testLazySend :: Test
testLazySend = TestCase $ tcpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) (C.take 1024 strictTestMsg)
      client sock = NSBL.send sock lazyTestMsg >>= (@=?) 1024

      -- message containing too many chunks to be sent in one system call
      lazyTestMsg = let alphabet = map C.singleton ['a'..'z']
                    in L.fromChunks (concat (replicate 100 alphabet))

      strictTestMsg = C.concat . L.toChunks $ lazyTestMsg

------------------------------------------------------------------------
-- Tests

testSend :: Test
testSend = TestCase $ tcpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) testMsg
      client sock = send sock testMsg

testSendAll :: Test
testSendAll = TestCase $ tcpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) testMsg
      client sock = sendAll sock testMsg

testSendTo :: Test
testSendTo = TestCase $ udpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) testMsg
      client sock = do addr <- inet_addr serverAddr
                       sendTo sock testMsg (SockAddrInet serverPort addr)

testSendAllTo :: Test
testSendAllTo = TestCase $ udpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) testMsg
      client sock = do addr <- inet_addr serverAddr
                       sendAllTo sock testMsg (SockAddrInet serverPort addr)

testSendMany :: Test
testSendMany = TestCase $ tcpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) (S.append seg1 seg2)
      client sock = sendMany sock [seg1, seg2]

      seg1 = C.pack "This is a "
      seg2 = C.pack "test message."

testSendManyTo :: Test
testSendManyTo = TestCase $ udpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) (S.append seg1 seg2)
      client sock = do addr <- inet_addr serverAddr
                       sendManyTo sock [seg1, seg2]
                           (SockAddrInet serverPort addr)

      seg1 = C.pack "This is a "
      seg2 = C.pack "test message."

testRecv :: Test
testRecv = TestCase $ tcpTest client server
    where
      server sock = recv sock 1024 >>= (@=?) testMsg
      client sock = send sock testMsg

testOverFlowRecv :: Test
testOverFlowRecv = TestCase $ tcpTest client server
    where
      server sock = do seg1 <- recv sock (S.length testMsg - 3)
                       seg2 <- recv sock 1024
                       let msg = S.append seg1 seg2
                       testMsg @=? msg

      client sock = send sock testMsg

testRecvFrom :: Test
testRecvFrom = TestCase $ tcpTest client server
    where
      server sock = do (msg, _) <- recvFrom sock 1024
                       testMsg @=? msg

      client sock = send sock testMsg

testOverFlowRecvFrom :: Test
testOverFlowRecvFrom = TestCase $ tcpTest client server
    where
      server sock = do (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                       (seg2, _) <- recvFrom sock 1024
                       let msg = S.append seg1 seg2
                       testMsg @=? msg

      client sock = send sock testMsg

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
        bracket serverSetup sClose $ \sock -> do
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
    bracket before after thing
    `catch` \ (e :: SomeException) -> throwTo tid e

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = withSocketsDo $ do
    counts <- runTestTT tests
    when (errors counts + failures counts > 0) exitFailure

tests :: Test
tests = TestList [ TestLabel "testLazySend" testLazySend
                 , TestLabel "testSend" testSend
                 , TestLabel "testSendAll" testSendAll
                 , TestLabel "testSendTo" testSendTo
                 , TestLabel "testSendAllTo" testSendAllTo
                 , TestLabel "testSendMany" testSendMany
                 , TestLabel "testSendManyTo" testSendManyTo
                 , TestLabel "testRecv" testRecv
                 , TestLabel "testOverFlowRecv" testOverFlowRecv
                 , TestLabel "testRecvFrom" testRecvFrom
                 , TestLabel "testOverFlowRecvFrom" testOverFlowRecvFrom
                 ]
