-- | Tests for things that didn't work in the past.
module Main where

import Network.Socket
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)

------------------------------------------------------------------------
-- Tests

-- Used to segfault on OS X 10.8.2 due to AI_NUMERICSERV being set
-- without a service being set. This is a OS X bug.
testGetAddrInfo :: IO ()
testGetAddrInfo = do
    let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
    _ <- getAddrInfo (Just hints) (Just "localhost") Nothing
    return ()

------------------------------------------------------------------------
-- List of all tests

tests :: [Test]
tests =
    [ testCase "testGetAddrInfo" testGetAddrInfo
    ]

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = withSocketsDo $ defaultMain tests
