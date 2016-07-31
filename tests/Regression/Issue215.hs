{-
- Network.ByteString.recv should not throw an error when its peer closes the
- socket, but should return an empty ByteString.
-}
module Regression.Issue215 (main) where

import Network                   (listenOn, PortID(..))
import Network.Socket     hiding (recv)
import Network.Socket.ByteString (recv)
import System.IO.Error           (catchIOError)
import Test.HUnit                (assertFailure)

main :: IO ()
main = do
  s <- listenOn $ PortNumber 5000
  addr <- getSocketName s
  let MkSocket _ family _ _ _ = s

  cli <- socket family Stream 0
  connect cli addr

  (serv, _) <- accept s
  close s
  close serv

  catchIOError
    (recv cli 1 >> close cli)
    (\e -> close cli >> assertFailure ("client threw an IOError: " ++ show e))
