module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (when)
import Network.Socket hiding (recv, recvFrom, send)
import System.Exit (exitFailure)
import Test.HUnit (Counts(..), Test(..), (@=?), runTestTT)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

import Network.Socket.ByteString (recv, recvFrom, send, sendAll, sendMany)

port :: PortNumber
port = fromIntegral (3000 :: Int)

------------------------------------------------------------------------
-- Tests

testMsg :: S.ByteString
testMsg = C.pack "This is a test message."

testSendAll :: Test
testSendAll = TestCase $ connectedTest client server
    where
      server sock = do msg <- recv sock 1024
                       testMsg @=? msg

      client sock = sendAll sock testMsg

testSendMany :: Test
testSendMany = TestCase $ connectedTest client server
    where
      server sock = do msg <- recv sock 1024
                       S.append seg1 seg2 @=? msg

      client sock = sendMany sock [seg1, seg2]

      seg1 = C.pack "This is a "
      seg2 = C.pack "test message."

testRecv :: Test
testRecv = TestCase $ connectedTest client server
    where
      server sock = do msg <- recv sock 1024
                       testMsg @=? msg

      client sock = send sock testMsg

testOverFlowRecv :: Test
testOverFlowRecv = TestCase $ connectedTest client server
    where
      server sock = do seg1 <- recv sock (S.length testMsg - 3)
                       seg2 <- recv sock 1024
                       let msg = S.append seg1 seg2
                       testMsg @=? msg

      client sock = send sock testMsg

testRecvFrom :: Test
testRecvFrom = TestCase $ connectedTest client server
    where
      server sock = do (msg, _) <- recvFrom sock 1024
                       testMsg @=? msg

      client sock = send sock testMsg

testOverFlowRecvFrom :: Test
testOverFlowRecvFrom = TestCase $ connectedTest client server
    where
      server sock = do (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                       (seg2, _) <- recvFrom sock 1024
                       let msg = S.append seg1 seg2
                       testMsg @=? msg

      client sock = send sock testMsg

------------------------------------------------------------------------
-- Test helpers

-- | Run a client/server pair and synchronize them so that the server
-- is started before the client and the specified server action is
-- finished before the client closes the connection.
connectedTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
connectedTest clientAct serverAct = do
    barrier <- newEmptyMVar
    forkIO $ server barrier
    client barrier
  where
    server barrier = do
        addr <- inet_addr "127.0.0.1"
        bracket (socket AF_INET Stream defaultProtocol) sClose $ \sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet port addr)
            listen sock 1
            serverReady
            (clientSock, _) <- accept sock
            serverAct clientSock
            sClose clientSock
            putMVar barrier ()
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier ()

    client barrier = do
        takeMVar barrier
        bracket (socket AF_INET Stream defaultProtocol) sClose $ \sock -> do
            addr <- inet_addr "127.0.0.1"
            connect sock $ SockAddrInet port addr
            clientAct sock
            takeMVar barrier

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = withSocketsDo $ do
    counts <- runTestTT tests
    when (errors counts + failures counts > 0) exitFailure

tests :: Test
tests = TestList [TestLabel "testSendAll" testSendAll,
                  TestLabel "testSendMany" testSendMany,
                  TestLabel "testRecv" testRecv,
                  TestLabel "testOverFlowRecv" testOverFlowRecv,
                  TestLabel "testRecvFrom" testRecvFrom,
                  TestLabel "testOverFlowRecvFrom" testOverFlowRecvFrom]
