module Main (main) where

import Control.Monad (unless)
import Distribution.Simple (defaultMainWithHooks, runTests, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Utils (die)
import System.Cmd (system)
import System.Directory (doesDirectoryExist)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { runTests = runTests' }
    where
      runTests' _ _ _ lbi = do
          built <- doesDirectoryExist $ buildDir lbi
          unless built $ die "Run the 'build' command first."
          system "runhaskell -i./dist/build tests/Simple.hs"
          return ()
