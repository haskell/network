{-
- Network.ByteString.recv should not throw an error when its peer closes the
- socket, but should return an empty ByteString.
-}
module Regression.Issue215 (main) where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)
import System.IO.Error (catchIOError)
import Test.HUnit (assertFailure)

main :: IO ()
main = do
  addr:_ <- getAddrInfo Nothing (Just "127.0.0.1") (Just "5000")
  s <- socket AF_INET Stream 0
  setSocketOption s ReusePort 1
  bind s $ addrAddress addr
  listen s maxListenQueue

  cli <- socket AF_INET Stream 0
  connect cli $ addrAddress addr

  (serv, _) <- accept s
  close s
  close serv

  catchIOError
    (recv cli 1 >> close cli)
    (\e -> close cli >> assertFailure ("client threw an IOError: " ++ show e))
