{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.Socket.ByteString.Lazy
-- Copyright   : (c) Bryan O'Sullivan 2009
-- License     : BSD-style
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- This module provides access to the BSD /socket/ interface.  For detailed
-- documentation, consult your favorite POSIX socket reference. All functions
-- communicate failures by converting the error number to an
-- 'System.IO.Error.IOError'.
--
-- This module is made to be imported with "Network.Socket" like so:
--
-- > import Network.Socket
-- > import Network.Socket.ByteString.Lazy
-- > import Prelude hiding (getContents)
module Network.Socket.ByteString.Lazy (
    -- * Send data to a socket
    send,
    sendAll,
    sendWithFds,
    sendWith,
    sendAllWith,

    -- * Receive data from a socket
    getContents,
    recv,
    recvWith,
) where

import Control.Concurrent.STM (STM)
import Data.ByteString.Lazy.Internal (
    ByteString (..),
    defaultChunkSize,
 )
import Network.Socket (ShutdownCmd (..), shutdown)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Types (Fd (..))
import Prelude hiding (getContents)

#if defined(mingw32_HOST_OS)
import Network.Socket.ByteString.Lazy.Windows  (send, sendAll)
#else
import Network.Socket.ByteString.Lazy.Posix    (send, sendAll)
#endif

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket.ByteString as N
import Network.Socket.Imports
import Network.Socket.Types
#if !defined(mingw32_HOST_OS)
import Network.Socket.Wait (withWritable)
#endif

-- | Send data and file descriptors over a UNIX-domain socket in
--   a single system call. This function does not work on Windows.
sendWithFds
    :: Socket
    -- ^ Socket
    -> ByteString
    -- ^ Data to send
    -> [Fd]
    -- ^ File descriptors
    -> IO ()
sendWithFds s lbs fds = N.sendManyWithFds s (L.toChunks lbs) fds

-- | 'send' which the caller can give up on.  See
--   'Network.Socket.withWritable' for what is left of the stream after
--   giving up.
--
--   @since 3.2.12.0
sendWith :: Socket -> ByteString -> STM a -> IO (Either a Int64)
#if defined(mingw32_HOST_OS)
-- 'send' sends the first chunk here, and so does this, through the
-- strict one, whose give-up cancels its own operation alone.
sendWith s lbs giveUp = fmap fromIntegral <$> N.sendWith s chunk giveUp
  where
    chunk = case L.toChunks lbs of
        [] -> S.empty
        x : _ -> x
#else
sendWith s lbs giveUp = withWritable s giveUp $ send s lbs
#endif

-- | 'sendAll' which the caller can give up on.  Part of the data may
--   have been sent before it gives up, so a stream given up on is to be
--   closed.
--
--   @since 3.2.12.0
sendAllWith :: Socket -> ByteString -> STM a -> IO (Either a ())
sendAllWith s lbs = N.sendManyWith s (L.toChunks lbs)

-- -----------------------------------------------------------------------------
-- Receiving

-- | Receive data from the socket.  The socket must be in a connected
-- state.  Data is received on demand, in chunks; each chunk will be
-- sized to reflect the amount of data received by individual 'recv'
-- calls.
--
-- All remaining data from the socket is consumed.  When there is no
-- more data to be received, the receiving side of the socket is shut
-- down.  If there is an error and an exception is thrown, the socket
-- is not shut down.
getContents
    :: Socket
    -- ^ Connected socket
    -> IO ByteString
    -- ^ Data received
getContents s = loop
  where
    loop = unsafeInterleaveIO $ do
        sbs <- N.recv s defaultChunkSize
        if S.null sbs
            then do
                shutdown s ShutdownReceive `catchIOError` const (return ())
                return Empty
            else Chunk sbs <$> loop

-- | Receive data from the socket.  The socket must be in a connected
-- state.  This function may return fewer bytes than specified.  If
-- the received data is longer than the specified length, it may be
-- discarded depending on the type of socket.  This function may block
-- until a message arrives.
--
-- If there is no more data to be received, returns an empty 'ByteString'.
recv
    :: Socket
    -- ^ Connected socket
    -> Int64
    -- ^ Maximum number of bytes to receive
    -> IO ByteString
    -- ^ Data received
recv s nbytes = chunk <$> N.recv s (fromIntegral nbytes)
  where
    chunk k
        | S.null k = Empty
        | otherwise = Chunk k Empty

-- | 'recv' which the caller can give up on.
--
--   @since 3.2.12.0
recvWith :: Socket -> Int64 -> STM a -> IO (Either a ByteString)
recvWith s nbytes giveUp = fmap chunk <$> N.recvWith s (fromIntegral nbytes) giveUp
  where
    chunk k
        | S.null k = Empty
        | otherwise = Chunk k Empty
