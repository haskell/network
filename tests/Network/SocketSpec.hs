{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SocketSpec (main, spec) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (readMVar)
import Control.Monad
import Data.Maybe (fromJust)
import Network.Socket
import Network.Socket.ByteString
import Network.Test.Common
import System.Mem (performGC)

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
            withPort $ \portVar -> test (tcp serverAddr return portVar)
                { clientSetup = readMVar portVar >>= connect'
                }

    describe "bind" $ do
        let hints = defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
        it "successfully binds to an ipv4 socket" $ do
            addr:_ <- getAddrInfo (Just hints) (Just serverAddr) Nothing
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock $ addrAddress addr

{- This does not work on Windows and Linux.
        it "fails to bind to unknown ipv4 socket" $ do
            addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.3") Nothing
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock (addrAddress addr) `shouldThrow` anyIOException
-}

        it "successfully binds to an ipv6 socket" $ do
            addr:_ <- getAddrInfo (Just hints) (Just serverAddr6) Nothing
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock $ addrAddress addr

        it "fails to bind to unknown ipv6 socket" $ do
            addr:_ <- getAddrInfo (Just hints) (Just "::6") Nothing
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock (addrAddress addr) `shouldThrow` anyIOException

    describe "UserTimeout" $ do
        it "can be set" $ do
            when (isSupportedSocketOption UserTimeout) $ do
              sock <- socket AF_INET Stream defaultProtocol
              setSocketOption sock UserTimeout 1000
              getSocketOption sock UserTimeout `shouldReturn` 1000
              setSocketOption sock UserTimeout 2000
              getSocketOption sock UserTimeout `shouldReturn` 2000
              close sock

    describe "getAddrInfo" $ do
        it "works for IPv4 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
            AddrInfo{addrAddress = (SockAddrInet _ hostAddr)}:_ <-
                getAddrInfo (Just hints) (Just "127.128.129.130") Nothing
            hostAddressToTuple hostAddr `shouldBe` (0x7f, 0x80, 0x81, 0x82)

        it "works for IPv6 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
                host = "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
            AddrInfo{addrAddress = (SockAddrInet6 _ _ hostAddr _)}:_ <-
                getAddrInfo (Just hints) (Just host) Nothing
            hostAddress6ToTuple hostAddr
                `shouldBe` (0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334)

        it "does not cause segfault on macOS 10.8.2 due to AI_NUMERICSERV" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
            void $ getAddrInfo (Just hints) (Just "localhost") Nothing

#if defined(mingw32_HOST_OS)
    let lpdevname = "loopback_0"
#elif defined(darwin_HOST_OS) || defined(freebsd_HOST_OS)
    let lpdevname = "lo0"
#else
    let lpdevname = "lo"
#endif

    describe "ifNameToIndex and ifIndexToName" $ do
        it "convert a name to an index and back" $
            do
            n <- ifNameToIndex lpdevname
            n `shouldNotBe` Nothing
            ifIndexToName (fromJust n) `shouldReturn` Just lpdevname

    describe "socket" $ do
        let gc = do
                threadDelay 100000
                performGC
            connect' = do
                threadDelay 200000
                sock <- socket AF_INET Stream defaultProtocol
                connect sock $ SockAddrInet 6000 $ tupleToHostAddress (127, 0, 0, 1)
        it "should not be GCed while blocking" $ do
            sock <- socket AF_INET Stream defaultProtocol
            setSocketOption sock ReuseAddr 1
            bind sock $ SockAddrInet 6000 $ tupleToHostAddress (127, 0, 0, 1)
            listen sock 1
            _ <- forkIO gc
            _ <- forkIO connect'
            (_sock', addr) <- accept sock
            -- check if an exception is not thrown.
            isSupportedSockAddr addr `shouldBe` True

#if !defined(mingw32_HOST_OS)
    when isUnixDomainSocketAvailable $ do
        context "unix sockets" $ do
            it "basic unix sockets end-to-end" $ do
                let client sock = send sock testMsg
                    server (sock, addr) = do
                        recv sock 1024 `shouldReturn` testMsg
                        addr `shouldBe` (SockAddrUnix "")
                test . setClientAction client $ unixWithUnlink unixAddr server
#endif

#ifdef linux_HOST_OS
            it "can end-to-end with an abstract socket" $ do
                let
                    abstractAddress = toEnum 0:"/haskell/network/abstract"
                    client sock = send sock testMsg
                    server (sock, addr) = do
                        recv sock 1024 `shouldReturn` testMsg
                        addr `shouldBe` (SockAddrUnix "")
                test . setClientAction client $
                    unix abstractAddress (const $ return ()) $ server
            it "safely throws an exception" $ do
                when isUnixDomainSocketAvailable $ do
                    let abstractAddress = toEnum 0:"/haskell/network/abstract-longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglong"
                    sock <- socket AF_UNIX Stream defaultProtocol
                    bind sock (SockAddrUnix abstractAddress) `shouldThrow` anyErrorCall
#endif

#if !defined(mingw32_HOST_OS)
            describe "socketPair" $ do
                it "can send and recieve bi-directionally" $ do
                    (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
                    void $ send s1 testMsg
                    recv s2 1024 `shouldReturn` testMsg
                    void $ send s2 testMsg
                    recv s1 1024 `shouldReturn` testMsg

            describe "sendFd/recvFd" $ do
                it "can send and recieve a file descriptor" $ do
                    (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
                    (s3, s4) <- socketPair AF_UNIX Stream defaultProtocol
                    withFdSocket s1 $ \fd1 -> void $ sendFd s3 fd1
                    fd1' <- recvFd s4
                    s1' <- mkSocket fd1'
                    void $ send s1' testMsg
                    recv s2 1024 `shouldReturn` testMsg

        -- On various BSD systems the peer credentials are exchanged during
        -- connect(), and this does not happen with `socketpair()`.  Therefore,
        -- we must actually set up a listener and connect, rather than use a
        -- socketpair().
        --
        describe "getPeerCredential" $ do
            it "can return something" $ do
                -- It would be useful to check that we did not get garbage
                -- back, but rather the actual uid of the test program.  For
                -- that we'd need System.Posix.User, but that is not available
                -- under Windows.  For now, accept the risk that we did not get
                -- the right answer.
                --
                let server (sock, _) = do
                        (_, uid, _) <- getPeerCredential sock
                        uid `shouldNotBe` Nothing
                    client sock = do
                        (_, uid, _) <- getPeerCredential sock
                        uid `shouldNotBe` Nothing
                test . setClientAction client $ unixWithUnlink unixAddr server
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
#endif

    describe "gracefulClose" $ do
        it "does not send TCP RST back" $ do
            let server sock = do
                    void $ recv sock 1024 -- receiving "GOAWAY"
                    gracefulClose sock 3000
                client sock = do
                    sendAll sock "GOAWAY"
                    threadDelay 10000
                    sendAll sock "PING"
                    threadDelay 10000
                    void $ recv sock 1024
            tcpTest client server

    describe "socketToFd" $ do
        it "socketToFd can send using fd" $ do
            let server sock = do
                    void $ recv sock 1024
                client sock = do
                    fd <- socketToFd sock
                    s <- mkSocket fd
                    sendAll s "HELLO WORLD"
            tcpTest client server

    describe "getNameInfo" $ do
        it "works for IPv4 address" $ do
            let addr = SockAddrInet 80 (tupleToHostAddress (127, 0, 0, 1))
            (hn_m, sn_m) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr

            hn_m `shouldBe` (Just "127.0.0.1")
            sn_m `shouldBe` (Just "80")

        it "works for IPv6 address" $ do
            let addr = SockAddrInet6 80 0
                           (tupleToHostAddress6 (0x2001, 0x0db8, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7)) 0
            (hn_m, sn_m) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
            hn_m `shouldBe`(Just "2001:db8:2:3:4:5:6:7")
            sn_m `shouldBe` (Just "80")

        it "works for IPv6 address" $ do
            let addr = SockAddrInet6 80 0
                           (tupleToHostAddress6 (0x2001, 0x0db8, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7)) 0
            (hn_m, sn_m) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
            hn_m `shouldBe`(Just "2001:db8:2:3:4:5:6:7")
            sn_m `shouldBe` (Just "80")

        it "works for global multicast IPv6 address" $ do
            let addr = SockAddrInet6 80 0
                           (tupleToHostAddress6 (0xfe01, 0x0db8, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7)) 0
            (hn_m, sn_m) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
            hn_m `shouldBe`(Just "fe01:db8:2:3:4:5:6:7")
            sn_m `shouldBe` (Just "80")

    describe "show SocketAddr" $ do
        it "works for IPv4 address" $
            let addr = SockAddrInet 80 (tupleToHostAddress (127, 0, 0, 1)) in
            show addr `shouldBe` "127.0.0.1:80"

        it "works for IPv6 address" $
            let addr = SockAddrInet6 80 0
                           (tupleToHostAddress6 (0x2001, 0x0db8, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7)) 0 in
            show addr `shouldBe` "[2001:db8:2:3:4:5:6:7]:80"

        it "works for IPv6 address with zeros" $
            let addr = SockAddrInet6 80 0
                           (tupleToHostAddress6 (0x2001, 0x0db8, 0x2, 0x3, 0x0, 0x0, 0x0, 0x7)) 0 in
            show addr `shouldBe` "[2001:db8:2:3::7]:80"

        it "works for multicast IPv6 address with reserved scope" $ do
            let addr = SockAddrInet6 80 0
                           (tupleToHostAddress6 (0xff01, 0x1234, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7)) 0
            show addr `shouldBe` "[ff01:1234:2:3:4:5:6:7]:80"

