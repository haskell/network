{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- This example was copied directly from https://github.com/haskell-Z/stdio
-- It is a minimial HTTP servers intended to be benchmarked with standard
-- HTTP client benchmarking tools. For example:
--
-- wrk -c1000 -d10s http://127.0.0.1:8888   
-- siege -c 1000 -r 10 http://127.0.0.1:8888 
-- ab -r -k -c 100 -n 30000 http://127.0.0.1:8888/

import Network.Socket hiding (send, recv)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Foreign.ForeignPtr
import GHC.Exts
import GHC.ForeignPtr
import GHC.IO (IO(IO))
import Network.Socket.ByteString
import System.Environment
import Text.Read (readMaybe)

import qualified Data.ByteString as B

main :: IO ()
main = do
    portStr <- lookupEnv "PORT"
    let port = maybe 8888 id (readMaybe =<< portStr)
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet port 0
    listen sock 128
    cap <- getNumCapabilities
    capCounter <- newCounter
    onException (forever $ do
        (sock' , addr) <- accept sock
        c <- atomicAddCounter capCounter 1
        forkOn c $ do
            setSocketOption sock' NoDelay 1
            recvbuf <- mallocPlainForeignPtrBytes 2048
            -- we reuse buffer as golang does,
            -- since node uses slab, which is in fact a memory pool
            -- do not print ECONNRESET for fairness
            catch (echo sock' recvbuf) (\ (e::SomeException) -> return ()))
        (close sock)

  where
    echo sock recvbuf = loop >> close sock
      where
        loop = do
            r <- withForeignPtr recvbuf $ \ p -> do
                recvBuf sock p 2048
            when (r /= 0) $ do
                sendAll sock sendbuf
                loop

    sendbuf =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `B.append` (B.replicate 500 48)

atomicAddCounter :: Counter -> Int -> IO Int
{-# INLINE atomicAddCounter #-}
atomicAddCounter (Counter mba#) (I# x#) = IO $ \ s1# ->
    let !(# s2#, r #) = fetchAddIntArray# mba# 0# x# s1# in (# s2#, I# r #)

newCounter :: IO Counter
{-# INLINE newCounter #-}
newCounter = IO $ \ s1# -> case newByteArray# 8# s1# of
  (# s2#, arr #) -> case writeIntArray# arr 0# 0# s2# of
    s3# -> (# s3#, Counter arr #)

data Counter = Counter (MutableByteArray# RealWorld)
