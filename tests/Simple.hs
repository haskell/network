module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (when)
import Network.Socket hiding (recv)
import System.Exit (exitFailure)
import Test.HUnit (Counts(..), Test(..), (@=?), runTestTT)

import qualified Data.ByteString.Char8 as C

import Network.Socket.ByteString

port :: PortNumber
port = fromIntegral (3000 :: Int)

------------------------------------------------------------------------
-- Tests

testSendAll :: Test
testSendAll = TestCase $ mytest client server
    where
      server serverSock = do
        (sock, _) <- accept serverSock
        bytes <- recv sock 1024
        testData @=? bytes
        sClose sock

      client sock = do
        addr <- inet_addr "127.0.0.1"
        connect sock $ SockAddrInet port addr
        sendAll sock testData

      testData = C.pack "test"

------------------------------------------------------------------------
-- Test helpers

-- | Run a client/server pair and synchronize them so that the server
-- is started before the client and the specified server action is
-- finished before the client closes the connection.
mytest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
mytest clientAct serverAct = do
  barrier <- newEmptyMVar
  forkIO $ server barrier
  client barrier
    where
      server barrier = do
        addr <- inet_addr "127.0.0.1"
        bracket (socket AF_INET Stream defaultProtocol)
                sClose
                (\sock -> do
                   setSocketOption sock ReuseAddr 1
                   bindSocket sock (SockAddrInet port addr)
                   listen sock maxListenQueue
                   putMVar barrier ()
                   serverAct sock
                   putMVar barrier ())

      client barrier = do
        takeMVar barrier
        bracket (socket AF_INET Stream defaultProtocol)
                sClose
                (\sock -> clientAct sock >> takeMVar barrier)

main :: IO ()
main = withSocketsDo $ do
  counts <- runTestTT $ TestList [TestLabel "testSendAll" testSendAll]
  when (errors counts + failures counts > 0) exitFailure

