{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket.Internal
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A module containing semi-public 'Network.Socket' internals.
-- Modules which extend the 'Network.Socket' module will need to use
-- this module while ideally most users will be able to make do with
-- the public interface.
--
-----------------------------------------------------------------------------

#include "HsNet.h"

module Network.Socket.Internal
    (
    -- * Socket addresses
      HostAddress
#if defined(IPV6_SOCKET_SUPPORT)
    , HostAddress6
    , FlowInfo
    , ScopeID
#endif
    , PortNumber(..)
    , SockAddr(..)

    , peekSockAddr
    , pokeSockAddr
    , sizeOfSockAddr
    , sizeOfSockAddrByFamily
    , withSockAddr
    , withNewSockAddr

    -- * Protocol families
    , Family(..)

    -- * Socket error functions
#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)
    , c_getLastError
#endif
    , throwSocketError
    , throwSocketErrorCode

    -- * Guards for socket operations that may fail
    , throwSocketErrorIfMinus1_
    , throwSocketErrorIfMinus1Retry
    , throwSocketErrorIfMinus1RetryMayBlock
    , throwSocketErrorIfMinus1RetryNoBlock
    , throwSocketErrorIfRetry

    -- ** Guards that wait and retry if the operation would block
    -- | These guards are based on 'throwSocketErrorIfMinus1RetryMayBlock'.
    -- They wait for socket readiness if the action fails with @EWOULDBLOCK@
    -- or similar.
    , throwSocketErrorWaitRead
    , throwSocketErrorWaitWrite

    -- * Polling sockets
    -- | Like 'threadWaitRead' and 'threadWaitWrite', but these work for
    -- sockets, even on Windows.
    --
    -- It is undefined behavior to close the socket during a call to one of
    -- these functions.  This is because the socket descriptor may be reused by
    -- another thread before the wait operation actually begins.
    , sockWaitRead
    , sockWaitWrite

    -- * Initialization
    , withSocketsDo

    -- * Low-level helpers
    , zeroMemory
    ) where

import Data.Bits ( (.|.), shiftL, shiftR )
import Data.Word ( Word8, Word16, Word32 )
import Data.Typeable (Typeable)
import Foreign.C.Error
import Foreign.C.String ( castCharToCChar, peekCString )
#if __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types ( CInt(..), CSize(..) )
#else
import Foreign.C.Types ( CInt, CSize )
#endif
import Foreign.Marshal.Alloc ( allocaBytes )
import Foreign.Marshal.Array ( pokeArray, pokeArray0 )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(..) )

#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)

import Control.Concurrent.MVar
import Control.Exception ( finally )
#  if __GLASGOW_HASKELL__
#    if __GLASGOW_HASKELL__ >= 707
import GHC.IO.Exception ( IOErrorType(..) )
#    else
import GHC.IOBase ( IOErrorType(..) )
#    endif
#  endif
import Foreign.C.Types ( CChar )
import System.IO.Error ( ioeSetErrorString, mkIOError )
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket.WinSelect

#else

import GHC.Conc (threadWaitRead, threadWaitWrite)
import System.Posix.Types

#endif

import Network.Socket.Types

-- ---------------------------------------------------------------------
-- Guards for socket operations that may fail

-- | Throw an 'IOError' corresponding to the current socket error.
throwSocketError :: String  -- ^ textual description of the error location
                 -> IO a

-- | Like 'throwSocketError', but the error code is supplied as an argument.
--
-- On Windows, do not use errno.  Use a system error code instead.
throwSocketErrorCode :: String -> CInt -> IO a

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()

{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a
throwSocketErrorIfMinus1Retry = throwSocketErrorIfRetry (== -1)

{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.  Checks for operations that would block and
-- executes an alternative action before retrying in that case.
throwSocketErrorIfMinus1RetryMayBlock
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO b    -- ^ action to execute before retrying if an
               --   immediate retry would block
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a

{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, return 'Nothing'.
throwSocketErrorIfMinus1RetryNoBlock
    :: (Eq a, Num a) => String -> IO a -> IO (Maybe a)

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result that matches the predicate.
-- Retry in case of an interrupted operation.
throwSocketErrorIfRetry
    :: (a -> Bool) -- ^ Predicate that returns 'True' if the result
                   --   indicates failure
    -> String      -- ^ Textual description of the call site
    -> IO a        -- ^ The action to perform
    -> IO a

throwErrnoIfRetryNoBlock :: (a -> Bool) -> String -> IO a -> IO (Maybe a)
throwErrnoIfRetryNoBlock pred loc act =
    tryAgain
  where
    tryAgain = do
        r <- act
        if pred r
          then do
            err@(Errno n) <- getErrno
            if err == eINTR
              then tryAgain
              else if err == eWOULDBLOCK || err == eAGAIN
                     then return Nothing
                     else throwSocketErrorCode loc n
          else return (Just r)

#if defined(__GLASGOW_HASKELL__) && (!defined(HAVE_WINSOCK2_H) || defined(cygwin32_HOST_OS))

throwSocketErrorIfMinus1RetryMayBlock name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block

throwSocketErrorIfRetry = throwErrnoIfRetry

throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_

throwSocketError = throwErrno

throwSocketErrorCode loc errno =
    ioError (errnoToIOError loc (Errno errno) Nothing Nothing)

throwSocketErrorIfMinus1RetryNoBlock =
    throwErrnoIfRetryNoBlock (== -1)

#else

throwSocketErrorIfMinus1_ name act = do
  throwSocketErrorIfMinus1Retry name act
  return ()

# if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)

throwSocketErrorIfRetryOnError
    :: (a -> b)                 -- ^ Maps the raw return value.  This allows
                                --   the final return value to have a different
                                --   type than that used for error checking.
    -> (a -> Bool)              -- ^ Predicate returning 'True' on error
    -> String                   -- ^ Textual description of the call site
    -> (IO b -> CInt -> IO b)   -- ^ What to do on error.  Use
                                --   'throwSocketErrorCode' to propagate the
                                --   error, or call the continuation to
                                --   try again.
    -> IO a                     -- ^ The IO operation to execute
    -> IO b
throwSocketErrorIfRetryOnError rt pred loc onError act =
    tryWith onError1
  where
    tryWith handler = do
        r <- act
        if pred r
            then c_getLastError >>= handler
            else return (rt r)

    tryAgain = tryWith (onError tryAgain)

    -- Only handle WSANOTINITIALISED one time.
    onError1 #{const WSANOTINITIALISED} = do
        withSocketsDo (return ())
        tryAgain
    onError1 err = onError tryAgain err

throwSocketErrorIfRetry pred loc act =
    throwSocketErrorIfRetryOnError id pred loc onError act
  where
    onError _tryAgain = throwSocketErrorCode loc

throwSocketErrorIfMinus1RetryMayBlock loc onBlock act =
    throwSocketErrorIfRetryOnError id (== -1) loc onError act
  where
    onError tryAgain #{const WSAEWOULDBLOCK} =
        onBlock >> tryAgain
    onError _ err =
        throwSocketErrorCode loc err

throwSocketErrorIfMinus1RetryNoBlock loc act =
    throwSocketErrorIfRetryOnError Just (== -1) loc onError act
  where
    onError _ #{const WSAEWOULDBLOCK} =
        return Nothing
    onError _ err =
        throwSocketErrorCode loc err

throwSocketErrorCode name rc = do
    pstr <- c_getWSError rc
    str  <- peekCString pstr
#  if __GLASGOW_HASKELL__
    ioError (ioeSetErrorString (mkIOError OtherError name Nothing Nothing) str)
#  else
    ioError (userError (name ++ ": socket error - " ++ str))
#  endif

throwSocketError name =
    c_getLastError >>= throwSocketErrorCode name

foreign import CALLCONV unsafe "WSAGetLastError"
  c_getLastError :: IO CInt

foreign import ccall unsafe "getWSErrorDescr"
  c_getWSError :: CInt -> IO (Ptr CChar)


# else
throwSocketErrorIfMinus1RetryMayBlock name _ act
  = throwSocketErrorIfMinus1Retry name act
throwSocketErrorIfRetry = throwErrnoIfRetry
throwSocketError = throwErrno
throwSocketErrorCode loc errno =
    ioError (errnoToIOError loc (Errno errno) Nothing Nothing)
throwSocketErrorIfMinus1RetryNoBlock =
    throwErrnoIfRetryNoBlock (== -1)
# endif
#endif /* __GLASGOW_HASKELL */

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.
throwSocketErrorWaitRead :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitRead sock name io =
    throwSocketErrorIfMinus1RetryMayBlock name
        (sockWaitRead $ sockFd sock)
        io

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be write-ready,
-- and try again.
throwSocketErrorWaitWrite :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitWrite sock name io =
    throwSocketErrorIfMinus1RetryMayBlock name
        (sockWaitWrite $ sockFd sock)
        io

-- ---------------------------------------------------------------------------
-- Polling sockets

-- | Block until the socket is ready to be read.
--
-- If the socket is closed during the operation, 'sockWaitRead' will likely
-- either throw an 'IOError', or return successfully.
sockWaitRead :: CInt -> IO ()

-- | Block until the socket is ready to be written to.
--
-- If the socket is closed during the operation, 'sockWaitWrite' will likely
-- throw an 'IOError'.
sockWaitWrite :: CInt -> IO ()

#if defined(HAVE_WINSOCK2_H) && !defined(cygwin32_HOST_OS)

sockWaitRead  fd = throwSelectError_ "sockWaitRead"  $ select1 fd evtRead
sockWaitWrite fd = throwSelectError_ "sockWaitWrite" $ select1 fd evtWrite

-- TODO: reuse the throwSelectError in Network.Socket, rather than
-- duplicating code.
throwSelectError :: String -> IO (Either CInt Event) -> IO Event
throwSelectError loc act = do
    e <- act
    case e of
        Left #{const WSANOTINITIALISED} -> do
            withSocketsDo (return ())
            e2 <- act
            case e2 of
                Left err -> throwSocketErrorCode loc err
                Right ev -> return ev
        Left err -> throwSocketErrorCode loc err
        Right ev -> return ev

throwSelectError_ :: String -> IO (Either CInt Event) -> IO ()
throwSelectError_ loc act =
    throwSelectError loc act >> return ()

#else

sockWaitRead  = threadWaitRead  . Fd
sockWaitWrite = threadWaitWrite . Fd

#endif


-- ---------------------------------------------------------------------------
-- WinSock support

{-| On Windows, the networking subsystem has to be initialised before any
networking operations can be used.  This calls @WSAStartup@, runs the inner
action, then queues @WSACleanup@ to be called before the process terminates.
It is a harmless no-op on other systems.

In versions of the network package before 2.5, it was mandatory to
call 'withSocketsDo' explicitly:

> main = withSocketsDo $ do {...}

Now, the network package calls it automatically when needed.  However, it is a
good idea to continue using 'withSocketsDo' at the top of @main@, in case
another library requires Winsock to be initialized.
-}
withSocketsDo :: IO a -> IO a
#if !defined(WITH_WINSOCK)
withSocketsDo x = x
#else
withSocketsDo act = do
    x <- withWsaInitLock initWinSock
    if x /= 0
       then ioError (userError "Failed to initialise WinSock")
       else act `finally` withWsaInitLock shutdownWinSock

wsaInitLock :: MVar ()
wsaInitLock = unsafePerformIO $ newMVar ()
{-# NOINLINE wsaInitLock #-}

withWsaInitLock :: IO a -> IO a
withWsaInitLock act = withMVar wsaInitLock $ \_ -> act
{-# NOINLINE withWsaInitLock #-}

foreign import ccall unsafe "initWinSock" initWinSock :: IO Int
foreign import ccall unsafe "shutdownWinSock" shutdownWinSock :: IO ()

#endif
