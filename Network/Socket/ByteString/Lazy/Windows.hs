module Network.Socket.ByteString.Lazy.Windows
    -- * Send data to a socket
  ( send
  , sendAll
  ) where

import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as L

import qualified Network.Socket.ByteString as Socket
import           Network.Socket.Imports
import           Network.Socket.Types

-- -----------------------------------------------------------------------------
-- Sending
send ::
     Socket -- ^ Connected socket
  -> L.ByteString -- ^ Data to send
  -> IO Int64 -- ^ Number of bytes sent
send s lbs =
  case L.toChunks lbs of
    []    -> fromIntegral <$> Socket.send s S.empty
    (x:_) -> fromIntegral <$> Socket.send s x
  -- TODO: Consider doing nothing if the string is empty.

sendAll ::
     Socket -- ^ Connected socket
  -> L.ByteString -- ^ Data to send
  -> IO ()
sendAll s bs = do
  sent <- send s bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll s bs'
