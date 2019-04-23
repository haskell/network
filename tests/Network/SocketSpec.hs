{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SocketSpec (main, spec) where

import Control.Concurrent.MVar (readMVar)
import Control.Monad
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import Network.Test.Common

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "connect" $ do
        let
          hints = defaultHints { addrSocketType = Stream }
          connect' serverPort = do
              addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
              sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
              connect sock (addrAddress addr)
              return sock

        it "fails to connect and throws an IOException" $ do
            connect' (8080 :: Int) `shouldThrow` anyIOException

        it "successfully connects to a socket with no exception" $ do
            tcpTestUsingClient return return $ readMVar >=> connect'

    describe "UserTimeout" $ do
        it "can be set" $ do
            when (isSupportedSocketOption UserTimeout) $ do
              sock <- socket AF_INET Stream defaultProtocol
              setSocketOption sock UserTimeout 1000
              getSocketOption sock UserTimeout `shouldReturn` 1000
              setSocketOption sock UserTimeout 2000
              getSocketOption sock UserTimeout `shouldReturn` 2000
              close sock

    -- On various BSD systems the peer credentials are exchanged during
    -- connect(), and this does not happen with `socketpair()`.  Therefore,
    -- we must actually set up a listener and connect, rather than use a
    -- socketpair().
    --
    describe "getPeerCredential" $ do
        it "can return something" $ do
            when isUnixDomainSocketAvailable $ do
                -- It would be useful to check that we did not get garbage
                -- back, but rather the actual uid of the test program.  For
                -- that we'd need System.Posix.User, but that is not available
                -- under Windows.  For now, accept the risk that we did not get
                -- the right answer.
                --
                let client sock = do
                        (_, uid, _) <- getPeerCredential sock
                        uid `shouldNotBe` Nothing
                    server (sock, _) = do
                        (_, uid, _) <- getPeerCredential sock
                        uid `shouldNotBe` Nothing
                unixTest client server
        {- The below test fails on many *BSD systems, because the getsockopt()
           call that underlies getpeereid() does not have the same meaning for
           all address families, but the C-library was not checking that the
           provided sock is an AF_UNIX socket.  This will fixed some day, but
           we should not fail on those systems in the mean-time.  The upstream
           C-library fix is to call getsockname() and check the address family
           before calling `getpeereid()`.  We could duplicate that in our own
           code, and then this test would work on those platforms that have
           `getpeereid()` and not the SO_PEERCRED socket option.

        it "return nothing for non-UNIX-domain socket" $ do
            when isUnixDomainSocketAvailable $ do
                s <- socket AF_INET Stream defaultProtocol
                cred1 <- getPeerCredential s
                cred1 `shouldBe` (Nothing,Nothing,Nothing)
        -}

    describe "getAddrInfo" $ do
        it "works for IPv4 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
            AddrInfo{addrAddress = (SockAddrInet _ hostAddr)}:_ <-
                getAddrInfo (Just hints) (Just "127.128.129.130") Nothing
            hostAddressToTuple hostAddr `shouldBe` (0x7f, 0x80, 0x81, 0x82)

#if defined(IPV6_SOCKET_SUPPORT)
        it "works for IPv6 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
                host = "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
            AddrInfo{addrAddress = (SockAddrInet6 _ _ hostAddr _)}:_ <-
                getAddrInfo (Just hints) (Just host) Nothing
            hostAddress6ToTuple hostAddr
                `shouldBe` (0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334)
#endif

        it "does not cause segfault on macOS 10.8.2 due to AI_NUMERICSERV" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
            void $ getAddrInfo (Just hints) (Just "localhost") Nothing

    describe "unix sockets" $ do
        it "basic unix sockets end-to-end" $ do
            when isUnixDomainSocketAvailable $ do
                let client sock = send sock testMsg
                    server (sock, addr) = do
                      recv sock 1024 `shouldReturn` testMsg
                      addr `shouldBe` (SockAddrUnix "")
                unixTest client server
#ifdef linux_HOST_OS
        it "can end-to-end with an abstract socket" $ do
            when isUnixDomainSocketAvailable $ do
                let
                    abstractAddress = toEnum 0:"/haskell/network/abstract"
                    clientAct sock = send sock testMsg
                    server (sock, addr) = do
                      recv sock 1024 `shouldReturn` testMsg
                      addr `shouldBe` (SockAddrUnix "")
                unixTestWith abstractAddress (const $ return ()) clientAct server
        it "safely throws an exception" $ do
            when isUnixDomainSocketAvailable $ do
                let abstractAddress = toEnum 0:"/haskell/network/abstract-longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglong"
                sock <- socket AF_UNIX Stream defaultProtocol
                bind sock (SockAddrUnix abstractAddress) `shouldThrow` anyErrorCall
#endif
