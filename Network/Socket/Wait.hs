{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-- | Blocking socket operations which a caller can give up on.
--
--   Each takes an 'STM' action beside the operation.  Whichever
--   settles first decides the result: 'Left' when the caller gave up,
--   'Right' when the operation completed.  Nothing has to be closed or
--   interrupted to make the thread return, so neither the delivery of
--   @evtClose@ by @closeFdWith@ nor an asynchronous exception reaching
--   a blocking call is relied upon.
--
--   __On Windows these need the threaded runtime.__  There is no
--   readiness to wait for there, so the operation is started and then
--   cancelled, and the thread it runs in blocks in a foreign call.
--   Without @-threaded@ that blocks every other thread too, including
--   the one which would have given up, and nothing returns at all.
module Network.Socket.Wait (
    withReadable,
    acceptWith,
    recvBufWith,
    recvBufFromWith,
    isClosedSocketError,
    isTransientAcceptError,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Foreign.C.Error (
    Errno (..),
    eCONNABORTED,
    eHOSTDOWN,
    eHOSTUNREACH,
    eNETDOWN,
    eNETUNREACH,
    eNONET,
    eNOPROTOOPT,
    ePROTO,
 )
import GHC.IO.Exception (IOErrorType (..), ioe_errno)
import System.IO.Error (ioeGetErrorType)

import Network.Socket.Buffer
#if defined(mingw32_HOST_OS)
import Network.Socket.Cbits
import Network.Socket.Win32.Cancel
#else
import Network.Socket.STM
#endif
import Network.Socket.Imports
import Network.Socket.Syscall
import Network.Socket.Types

-- | Running an action once the socket is readable, unless the 'STM'
--   action fires first.  The 'STM' action is also looked at before
--   anything else, so a caller which has already given up never waits.
--   An operation which has completed wins a tie, so nothing it has
--   already received is thrown away.
--
--   __The action must be an operation on this socket which returns
--   without blocking once the socket is readable.__  The type does not
--   say so and nothing checks it.  On POSIX the action runs on the
--   calling thread after readiness and nothing interrupts it there: the
--   give-up ends the wait, not the action.  On Windows there is no
--   readiness to wait for, so the action is started at once on a thread
--   of its own and the give-up cancels it with @CancelIoEx@ on this
--   socket, which reaches nothing else -- an action blocked on anything
--   else would never end.
--
--   __One reader at a time__ is what keeps that condition.  A second
--   thread taking the data between the readiness and the action leaves
--   the action blocking on the socket, with the give-up no longer in
--   play.
--
--   __The 'STM' action is run more than once__, so it has to be a test
--   and not something which takes what it reads: it is looked at before
--   the wait and again as part of it, and 'Network.Socket.ByteString.recvWith'
--   looks at it once more before a read which would not block.  Reading
--   a 'TVar' is what it is for.
withReadable :: Socket -> STM a -> IO b -> IO (Either a b)
withReadable s giveUp act = do
    -- Looking at the give-up first: on Windows 'wait' starts the
    -- operation before it looks at any STM, and on POSIX this keeps a
    -- caller which has given up off a descriptor close may have freed.
    gaveUp <- atomically $ (Just <$> giveUp) `orElse` return Nothing
    case gaveUp of
        Just a -> return $ Left a
        Nothing -> wait
  where
#if !defined(mingw32_HOST_OS)
    wait = do
        (readable, cancelWait) <- waitAndCancelReadSocketSTM s
        ev <-
            atomically ((Left <$> giveUp) `orElse` (Right <$> readable))
                `E.finally` (cancelWait `E.catch` gone)
        either (return . Left) (const $ Right <$> act) ev

    -- The cancel can throw, and from a finally that would replace the
    -- exception which ended the wait -- the timeout the caller is waiting
    -- to see, say.  What it reports is a registration which has already
    -- gone, and there is nothing to do about that.
    gone :: E.IOException -> IO ()
    gone _ = return ()
#else
    wait = do
        (completed, cancelAct) <- windowsViaSTM s act
        -- The operation wins a tie.  It has already read into the
        -- caller's buffer by then, and answering Left would throw those
        -- bytes away -- which for a stream loses them for good.
        --
        -- onException because the call runs in a thread of its own here:
        -- an asynchronous exception arriving during the wait would
        -- otherwise leave it blocked in a foreign call, holding the
        -- socket and writing into a buffer the caller has let go of.
        ev <-
            atomically ((Right <$> completed) `orElse` (Left <$> giveUp))
                `E.onException` cancelAct
        case ev of
            Right b -> return $ Right b
            Left a -> do
                cancelAct
                return $ Left a
#endif

-- | 'accept' which the caller can give up on.
--
--   @since 3.2.10.0
acceptWith :: SocketAddress sa => Socket -> STM a -> IO (Either a (Socket, sa))
acceptWith s giveUp = withReadable s giveUp $ accept s

-- | 'recvBuf' which the caller can give up on.
--
--   @since 3.2.10.0
recvBufWith :: Socket -> Ptr Word8 -> Int -> STM a -> IO (Either a Int)
recvBufWith s ptr nbytes giveUp = withReadable s giveUp $ recvBuf s ptr nbytes

-- | 'recvBufFrom' which the caller can give up on.
--
--   @since 3.2.10.0
recvBufFromWith
    :: SocketAddress sa
    => Socket -> Ptr Word8 -> Int -> STM a -> IO (Either a (Int, sa))
recvBufFromWith s ptr nbytes giveUp =
    withReadable s giveUp $ recvBufFrom s ptr nbytes

-- | Whether this is the error a socket operation gives once the socket has
--   been closed under it.
--
--   A caller which stops when it is told also stops when the socket it was
--   waiting on goes away, and this is how that arrives.  It is not an
--   error to report: closing the socket is how a server was stopped
--   before there was anything to tell it, and callers which do that are
--   still out there.
--
--   @since 3.2.10.0
isClosedSocketError :: E.IOException -> Bool
isClosedSocketError e = ioeGetErrorType e == InvalidArgument || isWinsock e
  where
#if defined(mingw32_HOST_OS)
    -- closesocket gives WSAEINTR to a call already blocked on the socket
    -- and WSAENOTSOCK to one which starts afterwards.
    isWinsock x = ioe_errno x `elem` map Just [wsaEINTR, wsaENOTSOCK]
#else
    isWinsock _ = False
#endif

-- | Whether an exception from 'accept' is about the connection it was
--   accepting rather than about the listening socket.
--
--   The peer may reset the connection before it is accepted
--   (@ECONNABORTED@), and Linux reports a new socket's already-pending
--   network errors through @accept()@ as well; accept(2) asks for all of
--   these to be retried, like @EAGAIN@.  The connection has left the queue
--   by then, so a retry blocks for a new one rather than spinning on the
--   same failure.
--
--   Two which are not here:
--
--   * Running out of descriptors (@EMFILE@, @ENFILE@) is about the process,
--     not about one connection, and retrying at once spins: the listening
--     socket stays readable.  'System.IO.Error.isFullError' names it, and a
--     caller which retries it should wait first.
--
--   * @EOPNOTSUPP@ is on accept(2)'s list and is left off this one on
--     purpose: it is also what a listening socket which is not
--     'Network.Socket.Stream' answers, which is permanent, and a retry
--     would spin on it forever.
--
--   Winsock numbers its errors apart from @errno@, so on Windows a list
--   of its own is read as well.  @WSAEINTR@ is not on it: there it is
--   what @closesocket@ gives a blocked call, which 'isClosedSocketError'
--   takes.  @WSAENOBUFS@ is left off for the reason @EMFILE@ is.
--
--   @since 3.2.10.0
isTransientAcceptError :: E.IOException -> Bool
isTransientAcceptError e =
    ioeGetErrorType e == Interrupted || any is errnos || isWinsock e
  where
    -- An errno the platform does not have is Errno (-1), which would
    -- otherwise match every error carrying that.
    is (Errno n) = n /= -1 && ioe_errno e == Just n
    errnos =
        [ eCONNABORTED
        , eHOSTDOWN
        , eHOSTUNREACH
        , eNETDOWN
        , eNETUNREACH
        , eNONET
        , eNOPROTOOPT
        , ePROTO
        ]
#if defined(mingw32_HOST_OS)
    isWinsock x = ioe_errno x `elem` map Just codes
    codes =
        [ wsaECONNABORTED
        , wsaECONNRESET
        , wsaEHOSTDOWN
        , wsaEHOSTUNREACH
        , wsaENETDOWN
        , wsaENETRESET
        , wsaENETUNREACH
        , wsaENOPROTOOPT
        ]
#else
    isWinsock _ = False
#endif
