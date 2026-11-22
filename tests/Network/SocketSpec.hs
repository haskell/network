{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SocketSpec (main, spec) where

import Control.Concurrent (threadDelay, forkIO, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (bracket)
import qualified Control.Exception as E
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Data.List (nub)
import Data.Maybe (fromJust)
import Foreign.C.Types ()
#if defined(mingw32_HOST_OS)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (sizeOf)
#endif
import Network.Socket
import Network.Socket.ByteString
import Network.Test.Common
import GHC.Clock (getMonotonicTime)
import GHC.IO.Exception (IOErrorType (NoSuchThing, ResourceVanished))
import System.IO.Error (ioeGetErrorString, ioeGetErrorType, tryIOError)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
#if !defined(_WIN32)
import System.Posix.Types (Fd(..))
#endif
import System.Timeout (timeout)

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

-- Send through the given action to a port nobody has bound, then receive
-- on the same socket.  'Nothing' if the receive heard nothing in time.
bounce :: (Socket -> SockAddr -> IO ()) -> IO (Maybe ByteString)
bounce = bounceWith $ \sock giveUp -> fmap fst <$> recvFromWith sock 2048 giveUp

bounceWith
    :: (Socket -> STM () -> IO (Either () ByteString))
    -> (Socket -> SockAddr -> IO ())
    -> IO (Maybe ByteString)
bounceWith receive send' = do
    dead <- deadPort
    bracket (socket AF_INET Datagram defaultProtocol) close $ \sock -> do
        bind sock $ SockAddrInet 0 localhost
        send' sock dead
        -- Long enough for the unreachable to come back before the receive.
        waitFor 300000
        late <- registerDelay 300000
        r <- receive sock $ check =<< readTVar late
        return $ either (const Nothing) Just r
  where
    localhost = tupleToHostAddress (127, 0, 0, 1)
    waitFor us = do
        late <- registerDelay us
        atomically $ check =<< readTVar late
    -- Bind a port, read it, and let it go again.
    deadPort = do
        s <- socket AF_INET Datagram defaultProtocol
        bind s $ SockAddrInet 0 localhost
        sa <- getSocketName s
        close s
        return sa

-- | 'bounce', but with the receive waiting before the send: the
--   unreachable completes a receive in progress.
bounceWaiting :: IO (Maybe ByteString)
bounceWaiting = bracket (socket AF_INET Datagram defaultProtocol) close $ \sock -> do
    dead <- bracket (socket AF_INET Datagram defaultProtocol) close $ \s -> do
        bind s $ SockAddrInet 0 localhost
        getSocketName s
    bind sock $ SockAddrInet 0 localhost
    connect sock dead
    var <- newEmptyMVar
    void $ forkIO $ tryIOError (recv sock 2048) >>= putMVar var
    threadDelay 100000
    void $ send sock "bounce"
    r <- timeout 1000000 $ takeMVar var
    traverse (either ioError return) r
  where
    localhost = tupleToHostAddress (127, 0, 0, 1)

-- | A bounce which must raise the unreachable.  Windows stops delivering
--   unreachables for a while after many of them, so there a bounce which
--   heard nothing is tried again, and the test is pending if none hears it.
shouldBeUnreachable :: IO (Maybe ByteString) -> Expectation
shouldBeUnreachable act = go attempts
  where
    go n = do
        r <- tryIOError act
        case r of
            Left e -> e `shouldSatisfy` isPortUnreachableError
            Right (Just bs) -> expectationFailure $ "received " ++ show bs
            Right Nothing
                | n > 1 -> threadDelay 2000000 >> go (n - 1)
                | otherwise -> silence
#if defined(mingw32_HOST_OS)
    attempts = 3 :: Int
    silence = pendingWith "Windows delivered no ICMP unreachable"
#else
    attempts = 1 :: Int
    silence = expectationFailure "no ICMP unreachable"
#endif

-- | Two connected datagram sockets on the loopback: the reader, and the
--   writer.
datagramPair :: (Socket -> Socket -> IO a) -> IO a
datagramPair body =
    bracket (socket AF_INET Datagram defaultProtocol) close $ \r ->
    bracket (socket AF_INET Datagram defaultProtocol) close $ \w -> do
        bind r $ SockAddrInet 0 localhost
        bind w $ SockAddrInet 0 localhost
        getSocketName w >>= connect r
        getSocketName r >>= connect w
        body r w
  where
    localhost = tupleToHostAddress (127, 0, 0, 1)

#if !defined(mingw32_HOST_OS)
-- Two unbound Unix datagram sockets.
withDatagramPair :: (Socket -> Socket -> IO a) -> IO a
withDatagramPair body =
    bracket (socket AF_UNIX Datagram defaultProtocol) close $ \s1 ->
    bracket (socket AF_UNIX Datagram defaultProtocol) close $ \s2 ->
        body s1 s2
#endif

-- An action's result, and how long it took in seconds.
timed :: IO a -> IO (a, Double)
timed act = do
    t0 <- getMonotonicTime
    r <- act
    t1 <- getMonotonicTime
    return (r, t1 - t0)

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
            connect' (8003 :: Int) `shouldThrow` anyIOException

        it "successfully connects to a socket with no exception" $ do
            withPort $ \portVar -> test (tcp serverAddr return portVar)
                { clientSetup = readMVar portVar >>= connect'
                }

    describe "connectWith" $ do
        let
          hints = defaultHints { addrSocketType = Stream }
          connectWith' serverPort giveUp = do
              addr:_ <- getAddrInfo (Just hints) (Just serverAddr) (Just $ show serverPort)
              sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
              r <- connectWith sock (addrAddress addr) giveUp
              return (sock, r)

        it "connects as connect does" $ do
            withPort $ \portVar -> test (tcp serverAddr return portVar)
                { clientSetup = do
                    (sock, r) <- readMVar portVar >>= \p -> connectWith' p (retry :: STM ())
                    r `shouldBe` Right ()
                    return sock
                }

        it "throws as connect does" $ do
            connectWith' (8003 :: Int) (retry :: STM ()) `shouldThrow` anyIOException

        it "gives up when already told to" $ do
            (sock, r) <- connectWith' (8003 :: Int) (return ())
            close sock
            r `shouldBe` Left ()

        it "gives up on a connection which does not come" $ do
            -- Nothing answers here, unless the network says it is
            -- unreachable at once, which is not giving up but is fine.
            sock <- socket AF_INET Stream defaultProtocol
            late <- registerDelay 300000
            r <- tryIOError $ connectWith sock (SockAddrInet 9 $ tupleToHostAddress (10, 255, 255, 1)) $
                check =<< readTVar late
            close sock
            r `shouldSatisfy` either (const True) (== Left ())

    describe "sendBufMsgWith and recvBufMsgWith" $ do
        it "send and receive as sendBufMsg and recvBufMsg do" $ do
            let server sock = allocaBytes 1024 $ \buf -> do
                    r <- recvBufMsgWith sock [(buf, 1024)] 0 mempty (retry :: STM ())
                    case r of
                        Right (_, n, _, _) ->
                            BS.packCStringLen (castPtr buf, n) `shouldReturn` testMsg
                        Left () -> expectationFailure "gave up without being told"
                client sock addr = BU.unsafeUseAsCStringLen testMsg $ \(p, n) ->
                    sendBufMsgWith sock addr [(castPtr p, n)] [] mempty (retry :: STM ())
                        `shouldReturn` Right n
            udpTest client server

        it "recvBufMsgWith gives up when told to" $ do
            sock <- socket AF_INET Datagram defaultProtocol
            bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
            late <- registerDelay 100000
            r <- allocaBytes 1024 $ \buf ->
                recvBufMsgWith sock [(buf, 1024)] 0 mempty $ check =<< readTVar late
            close sock
            either (const True) (const False) r `shouldBe` True

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

#ifdef DEVELOPMENT
        it "successfully binds to an ipv6 socket" $ do
            addr:_ <- getAddrInfo (Just hints) (Just serverAddr6) Nothing
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock $ addrAddress addr

        it "fails to bind to unknown ipv6 socket" $ do
            addr:_ <- getAddrInfo (Just hints) (Just "::6") Nothing
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock (addrAddress addr) `shouldThrow` anyIOException
#endif

        it "successfully binds to a unix socket" $ do
            withSystemTempDirectory "haskell-network" $ \path -> do
                let sfile = path ++ "/socket-file"
                -- exist <- doesFileExist sfile
                -- when exist $ removeFile sfile
                -- removeFile sfile
                let addr = SockAddrUnix sfile
                when (isSupportedSockAddr addr) $ do
                    sock0 <- socket AF_UNIX Stream defaultProtocol
                    bind sock0 addr
                    listen sock0 1

                    sock1 <- socket AF_UNIX Stream defaultProtocol
                    tryIOError (bind sock1 addr) >>= \o -> case o of
                        Right () -> error "bind should have failed but succeeded"
                        _        -> return ()

                    close sock0
                    close sock1

    describe "UserTimeout" $ do
        it "can be set" $ do
            when (isSupportedSocketOption UserTimeout) $ do
              sock <- socket AF_INET Stream defaultProtocol
              setSocketOption sock UserTimeout 1000
              getSocketOption sock UserTimeout `shouldReturn` 1000
              setSocketOption sock UserTimeout 2000
              getSocketOption sock UserTimeout `shouldReturn` 2000
              close sock

    describe "DontFragment" $ do
        it "can be set" $ do
            sock <- socket AF_INET Stream defaultProtocol
            when (isSupportedSocketOption DontFragment) $
                setSocketOption sock DontFragment 1
            close sock

    describe "getAddrInfo" $ do
        it "works for IPv4 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
            AddrInfo{addrAddress = (SockAddrInet _ hostAddr)}:_ <-
                getAddrInfo (Just hints) (Just "127.128.129.130") Nothing
            hostAddressToTuple hostAddr `shouldBe` (0x7f, 0x80, 0x81, 0x82)

        -- On Windows this used to be a userError which lost the reason (#636).
        it "throws NoSuchThing with a description for a name it cannot resolve" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST] }
            r <- tryIOError $ getAddrInfo (Just hints) (Just "no-such-host.invalid") Nothing
            case r :: Either IOError [AddrInfo] of
                Left e -> do
                    ioeGetErrorType e `shouldBe` NoSuchThing
                    ioeGetErrorString e `shouldNotContain` "not supported"
                Right _ -> expectationFailure "resolved a name which is not numeric"

        it "works for IPv6 address" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_ADDRCONFIG] }
                host = "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
            AddrInfo{addrAddress = (SockAddrInet6 _ _ hostAddr _)}:_ <-
                getAddrInfo (Just hints) (Just host) Nothing
            hostAddress6ToTuple hostAddr
                `shouldBe` (0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334)

        it "does not cause segfault on macOS 10.8.2 due to AI_NUMERICSERV" $ do
            let hints = defaultHints { addrFlags = [AI_NUMERICSERV] }
            void (getAddrInfo (Just hints) (Just "localhost") Nothing :: IO [AddrInfo])

#if defined(mingw32_HOST_OS)
    let lpdevname = "loopback_0"
#elif defined(darwin_HOST_OS) || defined(freebsd_HOST_OS) || defined(openbsd_HOST_OS)
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

    describe "socketPair" $ do
        it "can send and recieve bi-directionally" $ do
            (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
            void $ send s1 testMsg
            recv s2 1024 `shouldReturn` testMsg
            void $ send s2 testMsg
            recv s1 1024 `shouldReturn` testMsg

        -- Windows' AF_UNIX pair now and then missed a close racing the
        -- recv (seen in C and the non-threaded RTS, rarely here).
        it "tells a waiting recv that the peer has closed" $
            replicateM_ 100 $ do
                (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
                got <- newEmptyMVar
                _ <- forkIO $ recv s2 1024 >>= putMVar got
                _ <- forkIO $ close s1
                timeout 5000000 (takeMVar got) `shouldReturn` Just ""
                close s2

#if !defined(mingw32_HOST_OS)
    when isUnixDomainSocketAvailable $ do
        context "unix sockets" $ do
            it "basic unix sockets end-to-end" $ do
                withSystemTempDirectory "haskell-network" $ \path -> do
                    let unixAddr = path ++ "/socket-file"
                    let client sock = send sock testMsg
                        server (sock, addr) = do
                            recv sock 1024 `shouldReturn` testMsg
                            addr `shouldBe` (SockAddrUnix "")
                    test . setClientAction client $ unixWithUnlink unixAddr server

            -- Linux returns no address for an unbound sender (#539).
            it "receives datagrams from an unbound socket" $
                withSystemTempDirectory "haskell-network" $ \path ->
                    withDatagramPair $ \server client -> do
                        let addr = SockAddrUnix $ path ++ "/socket-file"
                        bind server addr
                        void $ sendTo client testMsg addr
                        recvFrom server 1024 `shouldReturn` (testMsg, SockAddrUnix "")
                        void $ sendTo client testMsg addr
                        (from, msg, _, _) <- recvMsg server 1024 0 mempty
                        (from, msg) `shouldBe` (SockAddrUnix "", testMsg)
#endif

#ifdef linux_HOST_OS
            -- Abstract names end at the length, not at NUL (#377).
            it "returns the names of abstract sockets" $
                withDatagramPair $ \server client -> do
                    let name = SockAddrUnix "\0haskell/network/abstract-name"
                        from = SockAddrUnix "\0haskell\0network\0abstract-from"
                    bind server name
                    bind client from
                    getSocketName server `shouldReturn` name
                    void $ sendTo client testMsg name
                    recvFrom server 1024 `shouldReturn` (testMsg, from)

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
            describe "sendFd/recvFd" $ do
                it "can send and recieve a file descriptor" $ do
                    (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
                    (s3, s4) <- socketPair AF_UNIX Stream defaultProtocol
                    withFdSocket s1 $ \fd1 -> void $ sendFd s3 fd1
                    fd1' <- recvFd s4
                    getCloseOnExec fd1' `shouldReturn` True
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
                withSystemTempDirectory "haskell-network" $ \path -> do
                    let unixAddr = path ++ "/socket-file"
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

    -- A send to a port nobody has bound brings an ICMP unreachable back.
    -- POSIX hands it to the sender only once the socket is connected, and
    -- Winsock hands it to every datagram socket; a reply bouncing off a
    -- client which has gone would then end a server's receive loop.
    describe "an ICMP unreachable from a send of one's own" $ do
        let connectSend sock dead = connect sock dead >> void (send sock "bounce")
            recvMsg' sock giveUp =
                fmap (\(_, bs, _, _) -> bs) <$> recvMsgWith sock 2048 0 mempty giveUp
        it "is not heard by a socket which was never connected" $
            bounce (\sock dead -> void $ sendTo sock "bounce" dead)
                `shouldReturn` Nothing
        it "is heard by a connected one" $
            shouldBeUnreachable $ bounce connectSend
        it "is heard by a connected one through recv" $
            shouldBeUnreachable $
                bounceWith (\sock -> recvWith sock 2048) connectSend
        it "is heard by a connected one through recvMsg" $
            shouldBeUnreachable $ bounceWith recvMsg' connectSend
        it "is heard by a receive already waiting" $
            shouldBeUnreachable bounceWaiting

    -- Winsock fails such a receive, and POSIX truncates the datagram.
    describe "a datagram too large for the buffer" $ do
        let big = BS.replicate 4000 1
        it "is truncated when it is already there" $
            datagramPair $ \r w -> do
                void $ send w big
                void $ send w "next"
                fst <$> recvFrom r 2048 `shouldReturn` BS.take 2048 big
                fst <$> recvFrom r 2048 `shouldReturn` "next"
        it "is truncated when the receive is waiting for it" $
            datagramPair $ \r w -> do
                var <- newEmptyMVar
                void $ forkIO $ recv r 2048 >>= putMVar var
                threadDelay 100000
                void $ send w big
                takeMVar var `shouldReturn` BS.take 2048 big
        it "is truncated by recvMsg" $
            datagramPair $ \r w -> do
                void $ send w big
                (_, bs, _, _) <- recvMsg r 2048 0 mempty
                bs `shouldBe` BS.take 2048 big

    describe "a connection the peer resets" $ do
        -- The byte makes the reset follow the connect: an RST which beat it
        -- would fail the connect itself on Linux.
        let resetBy receive = tcpTest client server
              where
                server sock = do
                    void $ recv sock 1
                    setSockOpt sock Linger (StructLinger 1 0)
                client sock = do
                    sendAll sock "x"
                    receive sock `shouldThrow` isVanished
                isVanished e = ioeGetErrorType e == ResourceVanished
        it "is reported as ResourceVanished" $
            resetBy $ \sock -> recvWith sock 1024 retry
        it "is reported as ResourceVanished by a receive already waiting" $
            resetBy $ \sock -> recv sock 1024

    describe "recvBufNoWait" $ do
        it "tells nothing to read yet (-1) from the end of the data (0)" $ do
            (s1, s2) <- socketPair AF_UNIX Stream defaultProtocol
            allocaBytes 16 $ \buf -> do
                recvBufNoWait s2 buf 16 `shouldReturn` (-1)
                void $ send s1 "x"
                waitReadSocketSTM s2 >>= atomically
                recvBufNoWait s2 buf 16 `shouldReturn` 1
                close s1
                waitReadSocketSTM s2 >>= atomically
                recvBufNoWait s2 buf 16 `shouldReturn` 0
            close s2

    describe "gracefulClose" $ do
        it "does not send TCP RST back" $ do
            let server sock = do
                    void $ recv sock 1024 -- receiving "GOAWAY"
                    gracefulClose sock 300
                client sock = do
                    sendAll sock "GOAWAY"
                    threadDelay 10000
                    sendAll sock "PING"
                    threadDelay 10000
                    void $ recv sock 1024
            tcpTest client server

        it "waits about the timeout for a peer which does not close" $ do
            done <- newEmptyMVar
            let server sock = do
                    void $ recv sock 1024
                    (_, t) <- timed $ gracefulClose sock 200
                    putMVar done ()
                    t `shouldSatisfy` (\x -> x >= 0.19 && x < 1)
                client sock = do
                    sendAll sock "GOAWAY"
                    void $ recv sock 1024 -- the server's FIN
                    takeMVar done
            tcpTest client server

    describe "gracefulCloseWith" $ do
        it "tells that the peer closed" $ do
            let server sock = do
                    void $ recv sock 1024
                    gracefulCloseWith sock retry `shouldReturn` (Right PeerClosed :: Either () Drained)
                -- test closes the client only after the server action.
                client sock = do
                    sendAll sock "GOAWAY"
                    threadDelay 10000
                    sendAll sock "PING"
                    void $ recv sock 1024
                    close sock
            tcpTest client server

        it "gives up when told to, on a peer which does not close" $ do
            done <- newEmptyMVar
            let server sock = do
                    void $ recv sock 1024
                    late <- registerDelay 100000
                    (r, t) <- timed $ gracefulCloseWith sock (readTVar late >>= check >> return ("late" :: String))
                    putMVar done ()
                    r `shouldBe` Left "late"
                    t `shouldSatisfy` (\x -> x >= 0.09 && x < 1)
                client sock = do
                    sendAll sock "GOAWAY"
                    void $ recv sock 1024
                    takeMVar done
            tcpTest client server

        -- A read which waited here would block past the give-up (#590).
        it "gives up even if another thread takes the data" $ do
            done <- newEmptyMVar
            let server sock = do
                    void $ recv sock 1024
                    -- Spinning, it takes the data before the drain's read.
                    _ <- forkIO $ allocaBytes 1024 $ \buf ->
                        let steal = do
                                r <- tryIOError $ recvBufNoWait sock buf 1024
                                when (r == Right (-1)) $ yield >> steal
                         in steal
                    late <- registerDelay 200000
                    (r, t) <- timed $ gracefulCloseWith sock (readTVar late >>= check)
                    putMVar done ()
                    r `shouldBe` Left ()
                    t `shouldSatisfy` (\x -> x >= 0.19 && x < 1)
                client sock = do
                    sendAll sock "GOAWAY"
                    threadDelay 50000
                    sendAll sock "PING"
                    takeMVar done
            tcpTest client server

        it "cuts off a peer which keeps sending" $ do
            done <- newEmptyMVar
            let server sock = do
                    void $ recv sock 1024
                    r <- gracefulCloseWith sock retry
                    putMVar done ()
                    r `shouldBe` (Right DrainLimit :: Either () Drained)
                client sock = do
                    sendAll sock "GOAWAY"
                    -- The server's close may reset this.
                    void $ tryIOError $ sendAll sock $ BS.replicate (256 * 1024) 0
                    takeMVar done
            tcpTest client server

    describe "unsafeGracefulClose" $ do
        it "does not send TCP RST back" $ do
            let server sock = do
                    void $ recv sock 1024 -- receiving "GOAWAY"
                    unsafeGracefulClose sock 300
                client sock = do
                    sendAll sock "GOAWAY"
                    threadDelay 10000
                    sendAll sock "PING"
                    threadDelay 10000
                    void $ recv sock 1024
            tcpTest client server

    describe "giving up" $ do
        it "ends a receive which is told to stop, leaving the socket usable" $ do
            let server sock = do
                    stop <- newTVarIO False
                    done <- newEmptyMVar
                    void $ forkIO $ do
                        r <- recvWith sock 1024 (check =<< readTVar stop)
                        putMVar done r
                    threadDelay 100000
                    atomically $ writeTVar stop True
                    takeMVar done `shouldReturn` Left ()
                    -- Nothing was closed, so the socket is still the
                    -- caller's to read.
                    recv sock 1024 `shouldReturn` testMsg
                client sock = do
                    threadDelay 300000
                    void $ send sock testMsg
            tcpTest client server

        -- A wait which inherited the mask could not be cancelled.
        it "ends a receive which is told to stop under uninterruptibleMask_" $ do
            let server sock = do
                    late <- registerDelay 100000
                    t0 <- getMonotonicTime
                    r <- E.uninterruptibleMask_ $
                        recvWith sock 1024 (check =<< readTVar late)
                    t1 <- getMonotonicTime
                    r `shouldBe` Left ()
                    (t1 - t0) `shouldSatisfy` (< 0.4)
                -- Closing ends the poll, so a wait which cannot be
                -- cancelled ends too, and the test fails rather than hangs.
                client sock = threadDelay 800000 >> close sock
            tcpTest client server

        -- The peer's FIN does not make room to send (WSAPoll's POLLHUP).
        it "ends a send to a peer which has shut down and does not read" $ do
            let server sock = do
                    threadDelay 100000 -- the client's FIN
                    late <- registerDelay 300000
                    let chunk = BS.replicate 65536 0
                        loop = do
                            r <- sendAllWith sock chunk (check =<< readTVar late)
                            either return (const loop) r
                    loop `shouldReturn` ()
                client sock = do
                    shutdown sock ShutdownSend
                    threadDelay 900000
            tcpTest client server

        -- A poll for reading can end with SEND alone, from the other
        -- direction; it is not a reason to read.
        it "ends a receive while another thread sends" $ do
            let server sock = do
                    late <- registerDelay 300000
                    stop <- newTVarIO False
                    _ <- forkIO $
                        let send' = do
                                r <- sendAllWith sock "ping" (check =<< readTVar stop)
                                either return (const $ threadDelay 1000 >> send') r
                         in send'
                    r <- recvWith sock 1024 (check =<< readTVar late)
                    atomically $ writeTVar stop True
                    r `shouldBe` Left ()
                client sock = do
                    threadDelay 900000
                    close sock
            tcpTest client server

        -- A tie goes to the data: answering Left would leave bytes
        -- already received unread, which for a stream loses them.
        it "takes what is there even once the caller has given up" $ do
            let server sock = do
                    threadDelay 100000
                    gone <- newTVarIO True
                    recvWith sock 1024 (check =<< readTVar gone)
                        `shouldReturn` Right testMsg
                client sock = void $ send sock testMsg
            tcpTest client server

        it "ends an accept which is told to stop, leaving the socket usable" $
            withListener $ \sock port -> do
                stop <- newTVarIO False
                done <- newEmptyMVar
                void $ forkIO $ do
                    r <- acceptWith sock (check =<< readTVar stop)
                    putMVar done $
                        either (const True) (const False)
                            (r :: Either () (Socket, SockAddr))
                threadDelay 100000
                atomically $ writeTVar stop True
                takeMVar done `shouldReturn` True
                -- The listening socket was not closed to end that
                -- accept, so it still accepts.
                bracket (socket AF_INET Stream defaultProtocol) close $ \cli -> do
                    connect cli $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
                    bracket (fst <$> accept sock) close $ \_ -> return ()

        -- One connection wakes both waiters; the one which finds it taken
        -- must still keep to its give-up rather than wait for the next.
        it "keeps to the give-up of an accept which another thread beat" $
            withListener $ \sock port -> do
                stops <- replicateM 2 $ newTVarIO False
                done <- newEmptyMVar
                forM_ (zip [0 :: Int ..] stops) $ \(i, stop) -> forkIO $ do
                    r <- acceptWith sock (check =<< readTVar stop)
                    case r :: Either () (Socket, SockAddr) of
                        Left () -> putMVar done (i, False)
                        Right (s, _) -> close s >> putMVar done (i, True)
                threadDelay 100000
                bracket (socket AF_INET Stream defaultProtocol) close $ \cli -> do
                    connect cli $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
                    (winner, True) <- takeMVar done
                    let loser = 1 - winner
                    threadDelay 100000
                    atomically $ writeTVar (stops !! loser) True
                    timeout 1000000 (takeMVar done) `shouldReturn` Just (loser, False)

        it "tells a socket closed under an accept from other errors" $ do
            sock <- socket AF_INET Stream defaultProtocol
            bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
            listen sock 1
            close sock
            r <- tryIOError $ accept sock
            either isClosedSocketError (const False) r `shouldBe` True

    describe "waiting for readiness" $ do
        it "waits until data arrives, and leaves it to be read" $
            loopbackPair $ \c s -> do
                (ready, _) <- waitAndCancelReadSocketSTM s
                fires 100 ready `shouldReturn` False
                sendAll c "hello"
                fires 2000 ready `shouldReturn` True
                recv s 64 `shouldReturn` "hello"

        it "is ready at once when data is there" $
            loopbackPair $ \c s -> do
                sendAll c "hello"
                threadDelay 50000
                (ready, _) <- waitAndCancelReadSocketSTM s
                fires 2000 ready `shouldReturn` True

        it "is ready at once for writing" $
            loopbackPair $ \_ s -> do
                (ready, _) <- waitAndCancelWriteSocketSTM s
                fires 2000 ready `shouldReturn` True

        it "does not fire once cancelled, and the socket still works" $
            loopbackPair $ \c s -> do
                (ready, cancel) <- waitAndCancelReadSocketSTM s
                cancel
                sendAll c "after"
                fires 200 ready `shouldReturn` False
                recv s 64 `shouldReturn` "after"

        it "fires on a listening socket when a connection comes" $
            withListener $ \l port -> do
                (ready, _) <- waitAndCancelReadSocketSTM l
                fires 100 ready `shouldReturn` False
                bracket (socket AF_INET Stream defaultProtocol) close $ \c -> do
                    connect c $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
                    fires 2000 ready `shouldReturn` True
                    accept l >>= close . fst

        it "fires for a datagram, and leaves it to be read" $
            bracket (socket AF_INET Datagram defaultProtocol) close $ \u1 ->
            bracket (socket AF_INET Datagram defaultProtocol) close $ \u2 -> do
                let lo p = SockAddrInet p $ tupleToHostAddress (127, 0, 0, 1)
                bind u1 $ lo 0
                port <- socketPort u1
                (ready, _) <- waitAndCancelReadSocketSTM u1
                fires 100 ready `shouldReturn` False
                void $ sendTo u2 "dgram" (lo port)
                fires 2000 ready `shouldReturn` True
                fst <$> recvFrom u1 64 `shouldReturn` "dgram"

        it "survives many waits which finish at once or are cancelled" $
            loopbackPair $ \_ s -> do
                replicateM_ 1000 $ do
                    (ready, _) <- waitAndCancelWriteSocketSTM s
                    atomically ready
                    performGC
                replicateM_ 500 $ do
                    (_, cancel) <- waitAndCancelReadSocketSTM s
                    cancel

#if defined(mingw32_HOST_OS)
    -- It used to be a CULong, 32 bits on Win64 (#426).
    describe "withFdSocket" $
        it "gives the whole SOCKET, as wide as a pointer" $
            bracket (socket AF_INET Stream defaultProtocol) close $ \s ->
                withFdSocket s (return . sizeOf) `shouldReturn` sizeOf nullPtr

#endif
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

    describe "show Family" $ do
        it "works for pattern synonyms" $
            let fam = AF_UNSPEC in
            show fam `shouldBe` "AF_UNSPEC"

        it "works for unsupported" $
            let fam = GeneralFamily (-1) in
            show fam `shouldBe` "UnsupportedFamily"

        it "works for positive values" $
            let fam = GeneralFamily 300 in
            show fam `shouldBe` "GeneralFamily 300"

        it "works for negative values" $
            let fam = GeneralFamily (-300) in
            show fam `shouldBe` "GeneralFamily (-300)"

    describe "show SocketType" $ do
        it "works for pattern synonyms" $
            let socktype = NoSocketType in
            show socktype `shouldBe` "NoSocketType"

        it "works for unsupported" $
            let socktype = GeneralSocketType (-1) in
            show socktype `shouldBe` "UnsupportedSocketType"

        it "works for positive values" $
            let socktype = GeneralSocketType 300 in
            show socktype `shouldBe` "GeneralSocketType 300"

        it "works for negative values" $
            let socktype = GeneralSocketType (-300) in
            show socktype `shouldBe` "GeneralSocketType (-300)"

    describe "show SocketOptions" $ do
        it "works for pattern synonyms" $
            let opt = ReuseAddr in
            show opt `shouldBe` "ReuseAddr"

        it "works for unsupported" $
            let opt = SockOpt (-1) (-1) in
            show opt `shouldBe` "UnsupportedSocketOption"

        it "works for positive values" $
            let opt = SockOpt 300 300 in
            show opt `shouldBe` "SockOpt 300 300"

        it "works for negative values" $
            let opt = SockOpt (-300) (-300) in
            show opt `shouldBe` "SockOpt (-300) (-300)"

    describe "show CmsgId" $ do
        it "works for pattern synonyms" $
            let msgid = CmsgIdIPv6HopLimit in
            show msgid `shouldBe` "CmsgIdIPv6HopLimit"

        it "works for unsupported" $
            let msgid = CmsgId (-1) (-1) in
            show msgid `shouldBe` "UnsupportedCmsgId"

        it "works for positive values" $
            let msgid = CmsgId 300 300 in
            show msgid `shouldBe` "CmsgId 300 300"

        it "works for negative values" $
            let msgid = CmsgId (-300) (-300) in
            show msgid `shouldBe` "CmsgId (-300) (-300)"

#if !defined(_WIN32)
    describe "bijective encodeCmsg-decodeCmsg roundtrip equality" $ do
        it "holds for [Fd]" $ forAll genFds $
            \x -> (decodeCmsg . encodeCmsg $ x) == Just (x :: [Fd])
#endif

    describe "bijective read-show roundtrip equality" $ do
        it "holds for Family" $ forAll familyGen $
            \x -> (read . show $ x) == (x :: Family)

        it "holds for SocketType" $ forAll socktypeGen $
            \x -> (read . show $ x) == (x :: SocketType)

        it "holds for SocketOption" $ forAll sockoptGen $
            \x -> (read . show $ x) == (x :: SocketOption)

        it "holds for CmsgId" $ forAll cmsgidGen $
            \x -> (read . show $ x) == (x :: CmsgId)

    describe "hostname" $ do
        it "is not empty" $ do
          hostname <- getHostName
          hostname `shouldSatisfy` (not . null)

-- Type-specific generators with strong bias towards pattern synonyms

-- Generator combinator that biases elements of a given list and otherwise
-- applies a function to a given generator
biasedGen :: (Gen a -> Gen b) -> [b] -> Gen a -> Gen b
biasedGen f xs g = do
    useBias <- (arbitrary :: Gen Bool)
    if useBias
       then elements xs
       else f g

familyGen :: Gen Family
familyGen = biasedGen (fmap GeneralFamily) familyPatterns arbitrary

socktypeGen :: Gen SocketType
socktypeGen = biasedGen (fmap GeneralSocketType) socktypePatterns arbitrary

sockoptGen :: Gen SocketOption
sockoptGen = biasedGen (\g -> SockOpt <$> g <*> g) sockoptPatterns arbitrary

cmsgidGen :: Gen CmsgId
cmsgidGen = biasedGen (\g -> CmsgId <$> g <*> g) cmsgidPatterns arbitrary

#if !defined(_WIN32)
genFds :: Gen [Fd]
genFds = listOf (Fd <$> arbitrary)
#endif

-- pruned lists of pattern synonym values for each type to generate values from

familyPatterns :: [Family]
familyPatterns = nub
    [UnsupportedFamily
    ,AF_UNSPEC,AF_UNIX,AF_INET,AF_INET6,AF_IMPLINK,AF_PUP,AF_CHAOS
    ,AF_NS,AF_NBS,AF_ECMA,AF_DATAKIT,AF_CCITT,AF_SNA,AF_DECnet
    ,AF_DLI,AF_LAT,AF_HYLINK,AF_APPLETALK,AF_ROUTE,AF_NETBIOS
    ,AF_NIT,AF_802,AF_ISO,AF_OSI,AF_NETMAN,AF_X25,AF_AX25,AF_OSINET
    ,AF_GOSSIP,AF_IPX,Pseudo_AF_XTP,AF_CTF,AF_WAN,AF_SDL,AF_NETWARE
    ,AF_NDD,AF_INTF,AF_COIP,AF_CNT,Pseudo_AF_RTIP,Pseudo_AF_PIP
    ,AF_SIP,AF_ISDN,Pseudo_AF_KEY,AF_NATM,AF_ARP,Pseudo_AF_HDRCMPLT
    ,AF_ENCAP,AF_LINK,AF_RAW,AF_RIF,AF_NETROM,AF_BRIDGE,AF_ATMPVC
    ,AF_ROSE,AF_NETBEUI,AF_SECURITY,AF_PACKET,AF_ASH,AF_ECONET
    ,AF_ATMSVC,AF_IRDA,AF_PPPOX,AF_WANPIPE,AF_BLUETOOTH,AF_CAN]

socktypePatterns :: [SocketType]
socktypePatterns = nub
    [ UnsupportedSocketType
    , NoSocketType
    , Stream
    , Datagram
    , Raw
    , RDM
    , SeqPacket
    ]

sockoptPatterns :: [SocketOption]
sockoptPatterns = nub
    [UnsupportedSocketOption
    ,Debug,ReuseAddr,SoDomain,Type,SoProtocol,SoError,DontRoute
    ,Broadcast,SendBuffer,RecvBuffer,KeepAlive,KeepInit,OOBInline
    ,TimeToLive,MaxSegment,NoDelay,Cork,Linger,ReusePort
    ,RecvLowWater,SendLowWater,RecvTimeOut,SendTimeOut
    ,UseLoopBack,UserTimeout,IPv6Only
    ,RecvIPv4TTL,RecvIPv4TOS,RecvIPv4PktInfo,DontFragment
    ,RecvIPv6HopLimit,RecvIPv6TClass,RecvIPv6PktInfo]

cmsgidPatterns :: [CmsgId]
cmsgidPatterns = nub
    [ UnsupportedCmsgId
    , CmsgIdIPv4TTL
    , CmsgIdIPv6HopLimit
    , CmsgIdIPv4TOS
    , CmsgIdIPv6TClass
    , CmsgIdIPv4PktInfo
    , CmsgIdIPv6PktInfo
    , CmsgIdFds
    ]

-- A listening socket on the loopback, and the port it got.
withListener :: (Socket -> PortNumber -> IO a) -> IO a
withListener action =
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen sock 1
        port <- socketPort sock
        action sock port

-- A connected TCP pair on the loopback: the client side, then the server side.
loopbackPair :: (Socket -> Socket -> IO a) -> IO a
loopbackPair action = withListener $ \l port ->
    bracket (socket AF_INET Stream defaultProtocol) close $ \c -> do
        connect c $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
        bracket (fst <$> accept l) close $ action c

-- Whether the STM action fires within the given milliseconds.
fires :: Int -> STM () -> IO Bool
fires ms ready = maybe False (const True) <$> timeout (ms * 1000) (atomically ready)
