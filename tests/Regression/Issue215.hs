{-# LANGUAGE OverloadedStrings #-}
module Regression.Issue215 (main) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (void, forever, unless)
import qualified Data.ByteString as S
import Data.Function (fix)
import Network (listenOn, sClose, PortID(..), socketPort)
import Network.Socket
       (Socket, SockAddr(..), accept, PortNumber, socket, Family(AF_INET),
        SocketType(Stream), connect)
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket.ByteString (recv, send)
import System.IO.Error (catchIOError)
import Test.HUnit (assertFailure)

main :: IO ()
main = bracket (listenOn (PortNumber 0)) sClose $ \listener -> do
    port <- socketPort listener
    void . forkIO $ server listener
    catchIOError
      (client port)
      (\e -> assertFailure $ "client threw an IOError: " ++ show e)

server :: Socket -> IO ()
server listener = forever $ bracket
    (accept listener)
    (sClose . fst)
    (\(s, _) -> void $ send s "Hello World\n")

client :: PortID -> IO ()
client (PortNumber port) = do
    s <- connectTo "localhost" port
    fix $ \loop -> do
        bs <- recv s 1024
        unless (S.null bs) loop
client p = error $ "Invalid PortID: " ++ show p

connectTo :: String -> PortNumber -> IO Socket
connectTo hostname port = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (sClose)  -- only done if there's an error
        (\sock -> do
          he <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          return sock
        )
