{-
- Network.ByteString.recv should not throw an error when its peer closes the
- socket, but should return an empty ByteString.
-}
module Regression.Issue215 (main) where

import Network                   (listenOn, PortID(..))
import Network.BSD               (getHostByName, hostAddress)
import Network.Socket     hiding (recv)
import Network.Socket.ByteString (recv)
import System.IO.Error           (catchIOError)
import Test.HUnit                (assertFailure)

main :: IO ()
main = do
  s <- listenOn $ PortNumber 0
  he <- getHostByName "localhost"
  p <- socketPort s

  cli <- socket AF_INET Stream 0
  connect cli (SockAddrInet p (hostAddress he))

  (serv, _) <- accept s
  close s
  close serv

  catchIOError
    (recv cli 1 >> close cli)
    (\e -> close cli >> assertFailure ("client threw an IOError: " ++ show e))
