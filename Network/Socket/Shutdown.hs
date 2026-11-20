{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNetDef.h"

module Network.Socket.Shutdown (
    ShutdownCmd(..)
  , shutdown
  , gracefulClose
  , unsafeGracefulClose
  , Drained (..)
  , gracefulCloseWith
  , unsafeGracefulCloseWith
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, yield)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified System.IO.Error as E
import System.Timeout

import Network.Socket.Buffer
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types
import Network.Socket.Wait

-- | Which directions 'shutdown' closes.
data ShutdownCmd
    = -- | Receiving (@SHUT_RD@).
      ShutdownReceive
    | -- | Sending (@SHUT_WR@): the peer reads the end of the stream.
      ShutdownSend
    | -- | Both (@SHUT_RDWR@).
      ShutdownBoth

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

-- | Shut down one or both halves of the connection, depending on the
-- second argument to the function.  If the second argument is
-- 'ShutdownReceive', further receives are disallowed.  If it is
-- 'ShutdownSend', further sends are disallowed.  If it is
-- 'ShutdownBoth', further sends and receives are disallowed.
shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown s stype = shutdown' `annotateIOException` show s
  where
    shutdown' =
      void $ withFdSocket s $ \fd ->
        throwSocketErrorIfMinus1Retry_ "Network.Socket.shutdown" $
          c_shutdown fd $ sdownCmdToInt stype

foreign import CALLCONV unsafe "shutdown"
  c_shutdown :: CSocket -> CInt -> IO CInt

-- | Closing a socket gracefully.
--   This sends TCP FIN and reads and discards what the peer sends, up to
--   128 KiB, until its FIN arrives or the timeout expires: data left
--   unread would make the close send RST.
--   The second argument is the timeout in milliseconds.
--   In both normal cases and error cases, socket is deallocated finally.
--
--   Since: 3.1.1.0
gracefulClose :: Socket -> Int -> IO ()
gracefulClose = gracefulCloseBy close

-- | 'gracefulClose' which ends with 'unsafeClose' rather than 'close',
--   for a caller which knows that no other thread is waiting on the
--   socket.  The wait for the peer's FIN is this thread's own.
--
--   @since 3.2.12.0
unsafeGracefulClose :: Socket -> Int -> IO ()
unsafeGracefulClose = gracefulCloseBy unsafeClose

-- | How a graceful close which was not given up on ended.
--
--   @since 4.0.0.0
data Drained
    = -- | The peer's FIN arrived, or the connection was already gone.
      PeerClosed
    | -- | The peer sent 128 KiB without closing, and was cut off.
      DrainLimit
    deriving (Eq, Show)

-- | 'gracefulClose' which the caller gives up on through an 'STM'
--   action rather than a timeout: 'Left' when it did, as in
--   'recvBufWith'.  Either way, the socket is closed at the end.  A
--   deadline is a 'registerDelay'; a server can also cut short the
--   oldest of its pending closes by writing a 'TVar'.  A give-up which
--   has already fired ends the wait at once, even if the FIN is there.
--   'registerDelay' needs @-threaded@, as does the give-up on Windows.
--
--   @since 4.0.0.0
gracefulCloseWith :: Socket -> STM a -> IO (Either a Drained)
gracefulCloseWith = gracefulCloseWithBy close

-- | 'gracefulCloseWith' which ends with 'unsafeClose', as
--   'unsafeGracefulClose' does.
--
--   @since 4.0.0.0
unsafeGracefulCloseWith :: Socket -> STM a -> IO (Either a Drained)
unsafeGracefulCloseWith = gracefulCloseWithBy unsafeClose

gracefulCloseBy :: (Socket -> IO ()) -> Socket -> Int -> IO ()
gracefulCloseBy closer s tmout0
    | rtsSupportsBoundThreads = do
        -- millisecond to microsecond
        late <- registerDelay $ tmout0 * 1000
        void $ gracefulCloseWithBy closer s (check =<< readTVar late)
    | otherwise = finishBy closer s () $ recvEOFtimeout s tmout0

gracefulCloseWithBy :: (Socket -> IO ()) -> Socket -> STM a -> IO (Either a Drained)
gracefulCloseWithBy closer s giveUp =
    finishBy closer s (Right PeerClosed) $ drain s giveUp

-- Sending FIN, waiting with the given action, and closing however that
-- ends.  If FIN cannot be sent, the connection is gone: nothing to wait.
finishBy :: (Socket -> IO ()) -> Socket -> r -> (Ptr Word8 -> IO r) -> IO r
finishBy closer s gone wait =
    (sendRecvFIN `E.finally` closer s) `annotateIOException` show s
  where
    sendRecvFIN = do
        -- Sending TCP FIN.
        ex <- E.tryIOError $ shutdown s ShutdownSend
        case ex of
          -- Don't catch asynchronous exceptions
          Left _ -> return gone
          Right () -> do
              -- Giving CPU time to other threads hoping that
              -- FIN arrives meanwhile.
              yield
              -- Waiting TCP FIN.
              E.bracket (mallocBytes bufSize) free wait

-- Waiting for the peer's FIN, draining what arrives before it.  The
-- receive is given up on rather than interrupted: an asynchronous
-- exception does not reach a thread blocked in one on Windows.  The read
-- does not wait: if another thread took the data, this waits again, still
-- under the give-up, rather than block past it (#590).
drain :: Socket -> STM a -> Ptr Word8 -> IO (Either a Drained)
drain s giveUp buf = loop 0
  where
    loop n0 = do
        r <- E.try $ withReadable s giveUp $
            recvBufNoWaitOr (throwSocketError "Network.Socket.gracefulClose") s buf bufSize
        case r of
            -- An error, such as a reset: the connection is gone.
            Left (_ :: E.IOException) -> return $ Right PeerClosed
            Right (Left a) -> return $ Left a
            Right (Right 0) -> return $ Right PeerClosed
            Right (Right (-1)) -> loop n0
            Right (Right n1)
                | n < drainLimit -> loop n
                | otherwise -> return $ Right DrainLimit
              where
                n = n0 + n1

-- registerDelay needs -threaded, and so does the give-up on Windows --
-- where this cannot cut a receive short either, but nothing else works
-- there without it.
recvEOFtimeout :: Socket -> Int -> Ptr Word8 -> IO ()
-- millisecond to microsecond
recvEOFtimeout s tmout0 buf = void $ timeout (tmout0 * 1000) $ recvBuf s buf bufSize

-- Don't use 4092 here. The GHC runtime takes the global lock
-- if the length is over 3276 bytes in 32bit or 3272 bytes in 64bit.
bufSize :: Int
bufSize = 1024

-- Data left unread makes close send RST rather than FIN, so what has
-- arrived is drained -- but only this far: nothing is going to read it.
drainLimit :: Int
drainLimit = 128 * 1024
