{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.ByteStringSpec (main, spec) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString
import Network.Test.Common

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "send" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "checks -1 correctly on Windows" $ do
            sock <- socket AF_INET Stream defaultProtocol
            send sock "hello world" `shouldThrow` anyException

    describe "sendAll" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = sendAll sock testMsg
            tcpTest client server

    describe "sendTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock serverPort = do
                    let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
                    addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
                    sendTo sock testMsg $ addrAddress addr
            udpTest client server

    describe "sendAllTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock serverPort = do
                    let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
                    addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
                    sendAllTo sock testMsg $ addrAddress addr
            udpTest client server

    describe "sendMany" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` S.append seg1 seg2
                client sock = sendMany sock [seg1, seg2]

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            tcpTest client server

    describe "sendManyTo" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` S.append seg1 seg2
                client sock serverPort = do
                    let hints = defaultHints { addrFlags = [AI_NUMERICHOST], addrSocketType = Datagram }
                    addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
                    sendManyTo sock [seg1, seg2] $ addrAddress addr

                seg1 = C.pack "This is a "
                seg2 = C.pack "test message."
            udpTest client server

    describe "recv" $ do
        it "works well" $ do
            let server sock = recv sock 1024 `shouldReturn` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do
                    seg1 <- recv sock (S.length testMsg - 3)
                    seg2 <- recv sock 1024
                    let msg = S.append seg1 seg2
                    msg `shouldBe` testMsg
                client sock = send sock testMsg
            tcpTest client server

        it "checks -1 correctly on Windows" $ do
            sock <- socket AF_INET Stream defaultProtocol
            recv sock 1024 `shouldThrow` anyException

    describe "recvFrom" $ do
        it "works well" $ do
            let server sock = do
                    (msg, _) <- recvFrom sock 1024
                    testMsg `shouldBe` msg
                client sock = do
                    addr <- getPeerName sock
                    sendTo sock testMsg addr
            tcpTest client server

        it "can treat overflow" $ do
            let server sock = do
                    (seg1, _) <- recvFrom sock (S.length testMsg - 3)
                    (seg2, _) <- recvFrom sock 1024
                    let msg = S.append seg1 seg2
                    testMsg `shouldBe` msg
                client sock = send sock testMsg
            tcpTest client server
