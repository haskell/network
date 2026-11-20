{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.Win32.ByteString.Lazy (
    -- * Send data to a socket
    send,
    sendAll,
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket.ByteString as Socket
import Network.Socket.ByteString.IO (waitWhen0)
import Network.Socket.Imports
import Network.Socket.Types

-- -----------------------------------------------------------------------------
-- Sending
-- | Send data to the socket, as much as one system call takes, and
--   return the number of bytes sent: use 'sendAll' to send them all.  On
--   POSIX, it sends up to 1024 chunks, stopping after the one which
--   reaches 4 MiB; on Windows, only the first chunk.
send
    :: Socket
    -- ^ Connected socket
    -> L.ByteString
    -- ^ Data to send
    -> IO Int64
    -- ^ Number of bytes sent
send s lbs = case L.toChunks lbs of
    -- TODO: Consider doing nothing if the string is empty.
    [] -> fromIntegral <$> Socket.send s S.empty
    (x : _) -> fromIntegral <$> Socket.send s x

-- | Send all the data to the socket, calling 'send' until it is sent.
sendAll
    :: Socket
    -- ^ Connected socket
    -> L.ByteString
    -- ^ Data to send
    -> IO ()
sendAll _ "" = return ()
sendAll s bs0 = loop bs0
  where
    loop bs = do
        -- "send" throws an exception.
        sent <- send s bs
        waitWhen0 (fromIntegral sent) s
        when (sent /= L.length bs) $ loop $ L.drop sent bs
