module Main (main) where

import Distribution.Simple (defaultMainWithHooks, defaultUserHooks)

main :: IO ()
main = defaultMainWithHooks defaultUserHooks
