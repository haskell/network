{-# LANGUAGE BangPatterns #-}
module Network.Socket.ByteString.Lazy.Windows
    (
    -- * Send data to a socket
      send
    , sendAll
    ) where

import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

import qualified Network.Socket.ByteString as Socket
import Network.Socket.Types

-- -----------------------------------------------------------------------------
-- Sending

send :: Socket       -- ^ Connected socket
     -> L.ByteString  -- ^ Data to send
     -> IO Int64      -- ^ Number of bytes sent
send s lbs = do
  fromIntegral `fmap` case L.toChunks lbs of
      -- TODO: Consider doing nothing if the string is empty.
      []    -> Socket.send s S.empty
      (x:_) -> Socket.send s x

sendAll :: Socket        -- ^ Connected socket
        -> L.ByteString  -- ^ Data to send
        -> IO ()
sendAll s bs = do
  sent <- send s bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll s bs'
