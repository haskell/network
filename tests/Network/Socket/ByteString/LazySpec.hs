{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.ByteString.LazySpec (main, spec) where

import Prelude hiding (getContents)

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, check, readTVar, registerDelay, retry)
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Network.Socket
import Network.Socket.ByteString.Lazy
import Network.Test.Common

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "send" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    send sock lazyTestMsg `shouldThrow` anyException
            tcpTest client server

    describe "sendAll" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = sendAll sock lazyTestMsg
            tcpTest client server

        it "throws when closed" $ do
            let server _ = return ()
                client sock = do
                    close sock
                    sendAll sock lazyTestMsg `shouldThrow` anyException
            tcpTest client server

    describe "sendWith" $ do
        it "sends as send does" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = sendWith sock lazyTestMsg never `shouldReturn` Right (L.length lazyTestMsg)
            tcpTest client server

    describe "sendAllWith" $ do
        it "sends as sendAll does" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = sendAllWith sock lazyTestMsg never `shouldReturn` Right ()
            tcpTest client server

    describe "recvWith" $ do
        it "receives as recv does" $ do
            let server sock = recvWith sock 1024 never `shouldReturn` Right lazyTestMsg
                client sock = sendAll sock lazyTestMsg
            tcpTest client server

        it "gives up when told to, with nothing to receive" $ do
            let server sock = do
                    late <- registerDelay 100000
                    recvWith sock 1024 (check =<< readTVar late) `shouldReturn` Left ()
                client _ = threadDelay 500000
            tcpTest client server

    describe "getContents" $ do
        it "works well" $ do
            let server sock = getContents sock `shouldReturn` lazyTestMsg
                client sock = do
                    void $ send sock lazyTestMsg
                    shutdown sock ShutdownSend
            tcpTest client server

        it "returns empty string at EOF" $ do
            let client s = getContents s `shouldReturn` L.empty
                server s = shutdown s ShutdownSend
            tcpTest client server

    describe "recv" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` lazyTestMsg
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "throws when closed" $ do
            let server sock = do
                    close sock
                    recv sock 1024 `shouldThrow` anyException
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do
                    seg1 <- recv sock (L.length lazyTestMsg - 3)
                    seg2 <- recv sock 1024
                    let msg = L.append seg1 seg2
                    msg `shouldBe` lazyTestMsg
                client sock = send sock lazyTestMsg
            tcpTest client server

        it "returns empty string at EOF" $ do
            let client s = recv s 4096 `shouldReturn` L.empty
                server s = shutdown s ShutdownSend
            tcpTest client server

never :: STM ()
never = retry
