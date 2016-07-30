{-
- Network.ByteString.recv should not throw an error when its peer closes the
- socket, but should return an empty ByteString.
-}
module Regression.Issue215 (main) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.IO.Error (catchIOError)
import Test.HUnit (assertFailure)

main :: IO ()
main = do
  (s1,s2) <- NS.socketPair NS.AF_UNIX NS.Stream 0
  NS.close s1
  catchIOError
    (NSB.recv s2 1 >>= print >> NS.close s2)
    (\e -> assertFailure $ "client threw an IOError: " ++ show e)
