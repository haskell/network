#! /usr/bin/env runhaskell

> module Main (main) where

> import Distribution.Simple (defaultMainWithHooks, runTests, simpleUserHooks)
> import System.Cmd (system)

> main :: IO ()
> main = defaultMainWithHooks $ simpleUserHooks { runTests = runTests' } where
>   runTests' _ _ _ _ = do
>     system "runhaskell -i./dist/build tests/Simple.hs"
>     return ()
