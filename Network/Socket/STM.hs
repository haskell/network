module Network.Socket.STM where

import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket.Types
import System.Posix.Types

-- | STM action to wait until the socket is ready for reading.
waitReadSocketSTM :: Socket -> IO (STM ())
waitReadSocketSTM s = fst <$> waitAndCancelReadSocketSTM s

-- | STM action to wait until the socket is ready for reading and STM
--   action to cancel the waiting.
waitAndCancelReadSocketSTM :: Socket -> IO (STM (), IO ())
waitAndCancelReadSocketSTM s = withFdSocket s $ threadWaitReadSTM . Fd

-- | STM action to wait until the socket is ready for writing.
waitWriteSocketSTM :: Socket -> IO (STM ())
waitWriteSocketSTM s = fst <$> waitAndCancelWriteSocketSTM s

-- | STM action to wait until the socket is ready for writing and STM
--   action to cancel the waiting.
waitAndCancelWriteSocketSTM :: Socket -> IO (STM (), IO ())
waitAndCancelWriteSocketSTM s = withFdSocket s $ threadWaitWriteSTM . Fd
