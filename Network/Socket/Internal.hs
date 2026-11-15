{-# LANGUAGE CPP #-}
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
-- A module containing semi-public "Network.Socket" internals.
-- Modules which extend the "Network.Socket" module will need to use
-- this module while ideally most users will be able to make do with
-- the public interface.
--
-----------------------------------------------------------------------------

module Network.Socket.Internal
    (
    -- * Socket error functions
      throwSocketError
    , throwSocketErrorCode
#if defined(mingw32_HOST_OS)
    , P.c_getLastError
    , P.winsockErrorString
#endif

    -- * Guards for socket operations that may fail
    , throwSocketErrorIfMinus1_
    , throwSocketErrorIfMinus1Retry
    , throwSocketErrorIfMinus1Retry_
    , throwSocketErrorIfMinus1RetryMayBlock
#if defined(mingw32_HOST_OS)
    , P.throwSocketErrorIfMinus1ButRetry
#endif
    , annotateIOException
    -- ** Guards that wait and retry if the operation would block
    -- | These guards are based on 'throwSocketErrorIfMinus1RetryMayBlock'.
    -- They wait for socket readiness if the action fails with @EWOULDBLOCK@
    -- or similar.
    , throwSocketErrorWaitRead
    , throwSocketErrorWaitReadBut
    , throwSocketErrorWaitWrite
    , waitReadSocket
    , waitWriteSocket

    -- * Initialization
    , withSocketsDo

    -- * Null socket address type
    , NullSockAddr (..)

    -- * Low-level helpers
    , zeroMemory
    ) where

import qualified Foreign.C.Error as C
import GHC.IO.Exception (IOException (..))
import System.IO.Error (modifyIOError)

import Network.Socket.Imports
import Network.Socket.Types
#if defined(mingw32_HOST_OS)
import qualified Network.Socket.Win32.Internal as P
#else
import qualified Network.Socket.Posix.Internal as P
#endif

annotateIOException :: IO a -> String -> IO a
annotateIOException io anno = modifyIOError f io
  where
    f ioe = ioe { ioe_description = ioe_description ioe ++ " " ++ errname ++ anno }
      where
         errname = case ioe_errno ioe of
           Nothing -> ""
           Just n -> "[" ++ showErrno n ++ "] "

showErrno :: CInt -> String
showErrno n = case lookup (C.Errno n) errnoNames of
  Nothing -> show n
  Just name -> name

errnoNames :: [(C.Errno, String)]
errnoNames = [
    (C.eACCES,          "EACCES")
  , (C.eADDRINUSE,      "EADDRINUSE")
  , (C.eADDRNOTAVAIL,   "EADDRNOTAVAIL")
  , (C.eAFNOSUPPORT,    "EAFNOSUPPORT")
  , (C.eAGAIN,          "EAGAIN")
  , (C.eBADF,           "EBADF")
  , (C.eCONNABORTED,    "ECONNABORTED")
  , (C.eCONNRESET,      "ECONNRESET")
  , (C.eDESTADDRREQ,    "EDESTADDRREQ")
  , (C.eEXIST,          "EEXIST")
  , (C.eFAULT,          "EFAULT")
  , (C.eINTR,           "EINTR")
  , (C.eINVAL,          "EINVAL")
  , (C.eIO,             "EIO")
  , (C.eISCONN,         "EISCONN")
  , (C.eISDIR,          "EISDIR")
  , (C.eLOOP,           "ELOOP")
  , (C.eMFILE,          "EMFILE")
  , (C.eMSGSIZE,        "EMSGSIZE")
  , (C.eNAMETOOLONG,    "ENAMETOOLONG")
  , (C.eNETDOWN,        "ENETDOWN")
  , (C.eNETUNREACH,     "ENETUNREACH")
  , (C.eMFILE,          "EMFILE")
  , (C.eNFILE,          "ENFILE")
  , (C.eNOBUFS,         "ENOBUFS")
  , (C.eNOENT,          "ENOENT")
  , (C.eNOMEM,          "ENOMEM")
  , (C.eNOTCONN,        "ENOTCONN")
  , (C.eNOTDIR,         "ENOTDIR")
  , (C.eNOTSOCK,        "ENOTSOCK")
  , (C.eOPNOTSUPP,      "EOPNOTSUPP")
  , (C.ePIPE,           "EPIPE")
  , (C.ePROTONOSUPPORT, "EPROTONOSUPPORT")
  , (C.ePROTOTYPE,      "EPROTOTYPE")
  , (C.eROFS,           "EROFS")
  , (C.eTIMEDOUT,       "ETIMEDOUT")
  , (C.eWOULDBLOCK,     "EWOULDBLOCK")
  ]

-- ---------------------------------------------------------------------
-- Guards for socket operations that may fail

-- | Throw an 'IOError' corresponding to the current socket error.
throwSocketError :: String  -- ^ textual description of the error location
                 -> IO a
throwSocketError = P.throwSocketError

-- | Like 'throwSocketError', but the error code is supplied as an argument.
--
-- On Windows the code is a Winsock error code rather than an errno.  It
-- is kept in 'ioe_errno' all the same, and the error type is the one
-- the errno of the same name has elsewhere.
throwSocketErrorCode :: String -> CInt -> IO a
throwSocketErrorCode = P.throwSocketErrorCode

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
throwSocketErrorIfMinus1_ = P.throwSocketErrorIfMinus1_
{-# INLINE throwSocketErrorIfMinus1_ #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a
throwSocketErrorIfMinus1Retry = P.throwSocketErrorIfMinus1Retry
{-# INLINE throwSocketErrorIfMinus1Retry #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation. Discards the result of the IO action after
-- error handling.
throwSocketErrorIfMinus1Retry_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
throwSocketErrorIfMinus1Retry_ loc m =
    void $ throwSocketErrorIfMinus1Retry loc m
{-# SPECIALIZE throwSocketErrorIfMinus1Retry_ :: String -> IO CInt -> IO () #-}

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
throwSocketErrorIfMinus1RetryMayBlock = P.throwSocketErrorIfMinus1RetryMayBlock
{-# INLINE throwSocketErrorIfMinus1RetryMayBlock #-}

-- ---------------------------------------------------------------------

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.
throwSocketErrorWaitRead :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitRead s name io = withFdSocket s $ \fd ->
    throwSocketErrorIfMinus1RetryMayBlock name (waitReadSocket s fd) io

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.  If it fails with the error the user was expecting then
-- ignore the error
throwSocketErrorWaitReadBut :: (Eq a, Num a) => (CInt -> Bool) -> Socket -> String -> IO a -> IO a
throwSocketErrorWaitReadBut exempt s name io = withFdSocket s $ \fd ->
    P.throwSocketErrorIfMinus1RetryMayBlockBut exempt name (waitReadSocket s fd) io

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be write-ready,
-- and try again.
throwSocketErrorWaitWrite :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitWrite s name io = withFdSocket s $ \fd ->
    throwSocketErrorIfMinus1RetryMayBlock name (waitWriteSocket s fd) io

-- | Waiting for the socket to be readable, or writable: through the IO
--   manager on POSIX, and through an AFD poll on Windows.
waitReadSocket, waitWriteSocket :: Socket -> CSocket -> IO ()
waitReadSocket = P.waitReadSocket
waitWriteSocket = P.waitWriteSocket

-- ---------------------------------------------------------------------------
-- WinSock support

{-| With older versions of the @network@ library (version 2.6.0.2 or earlier)
on Windows operating systems,
the networking subsystem must be initialised using 'withSocketsDo' before
any networking operations can be used. eg.

> main = withSocketsDo $ do {...}

It is fine to nest calls to 'withSocketsDo', and to perform networking operations
after 'withSocketsDo' has returned.

'withSocketsDo' is not necessary for the current network library.
However, for compatibility with older versions on Windows, it is good practice
to always call 'withSocketsDo' (it's very cheap).
-}
withSocketsDo :: IO a -> IO a
withSocketsDo = P.withSocketsDo
{-# INLINE withSocketsDo #-}
