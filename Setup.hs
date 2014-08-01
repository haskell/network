module Main (main) where

import Distribution.Simple
import Distribution.System
import System.Environment

main :: IO ()
main = do
  msys <- lookupEnv "MSYSTEM"
  writeFile "log.txt" (show $ ("MSYSTEM",msys))
  case (msys,buildOS) of
    (Nothing, Windows) -> defaultMain -- on Windows without MSYS use preconfigured headers and params
    _ -> defaultMainWithHooks autoconfUserHooks
