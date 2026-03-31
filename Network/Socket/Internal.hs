{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "HsNetDef.h"

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
    , c_getLastError
#endif

    -- * Guards for socket operations that may fail
    , throwSocketErrorIfMinus1_
    , throwSocketErrorIfMinus1Retry
    , throwSocketErrorIfMinus1Retry_
    , throwSocketErrorIfMinus1RetryMayBlock
#if defined(mingw32_HOST_OS)
    , throwSocketErrorIfMinus1ButRetry
#endif
    , annotateIOException
    -- ** Guards that wait and retry if the operation would block
    -- | These guards are based on 'throwSocketErrorIfMinus1RetryMayBlock'.
    -- They wait for socket readiness if the action fails with @EWOULDBLOCK@
    -- or similar.
    , throwSocketErrorWaitRead
    , throwSocketErrorWaitReadBut
    , throwSocketErrorWaitWrite

    -- * Initialization
    , withSocketsDo

    -- * Null socket address type
    , NullSockAddr (..)

    -- * Low-level helpers
    , zeroMemory
    ) where

import GHC.Conc (threadWaitRead, threadWaitWrite)

#if defined(mingw32_HOST_OS)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
# if __GLASGOW_HASKELL__ >= 707
import GHC.IO.Exception (IOErrorType(..))
# else
import GHC.IOBase (IOErrorType(..))
# endif
import System.IO.Error (ioeSetErrorString, mkIOError)
#else
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1Retry,
                        throwErrnoIfMinus1RetryMayBlock, throwErrnoIfMinus1_,
                        Errno(..), errnoToIOError)
#endif

#if defined(mingw32_HOST_OS)
import Network.Socket.Cbits
#endif
import Network.Socket.Imports
import Network.Socket.Types

import qualified Foreign.C.Error as C
import GHC.IO.Exception (IOException (..))
import System.IO.Error (modifyIOError)

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
#if defined(mingw32_HOST_OS)
throwSocketError name =
    c_getLastError >>= throwSocketErrorCode name
#else
throwSocketError = throwErrno
#endif

-- | Like 'throwSocketError', but the error code is supplied as an argument.
--
-- On Windows, do not use errno.  Use a system error code instead.
throwSocketErrorCode :: String -> CInt -> IO a
#if defined(mingw32_HOST_OS)
throwSocketErrorCode name rc = do
    pstr <- c_getWSError rc
    str  <- peekCString pstr
    ioError (ioeSetErrorString (mkIOError OtherError name Nothing Nothing) str)
#else
throwSocketErrorCode loc errno =
    ioError (errnoToIOError loc (Errno errno) Nothing Nothing)
#endif

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@.  Discards the result of the
-- IO action after error handling.
throwSocketErrorIfMinus1_
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO ()
#if defined(mingw32_HOST_OS)
throwSocketErrorIfMinus1_ name act = do
  _ <- throwSocketErrorIfMinus1Retry name act
  return ()
#else
throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry
#endif

{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.
throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> IO a    -- ^ the 'IO' operation to be executed
    -> IO a
#if defined(mingw32_HOST_OS)
throwSocketErrorIfMinus1Retry
  = throwSocketErrorIfMinus1ButRetry (const False)
#else
throwSocketErrorIfMinus1_ = throwErrnoIfMinus1_
#endif

{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

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
#if defined(mingw32_HOST_OS)
throwSocketErrorIfMinus1RetryMayBlock name _ act
  = throwSocketErrorIfMinus1Retry name act
#else
throwSocketErrorIfMinus1RetryMayBlock name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block
#endif

{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}


-- | Throw an 'IOError' corresponding to the current socket error if
-- the IO action returns a result of @-1@, but retries in case of an
-- interrupted operation.  Checks for operations that would block and
-- executes an alternative action before retrying in that case.  If the error
-- is one handled by the exempt filter then ignore it and return the error code.
throwSocketErrorIfMinus1RetryMayBlockBut
    :: (Eq a, Num a)
    => (CInt -> Bool) -- ^ exception exempt filter
    -> String         -- ^ textual description of the location
    -> IO b           -- ^ action to execute before retrying if an
                      --   immediate retry would block
    -> IO a           -- ^ the 'IO' operation to be executed
    -> IO a
#if defined(mingw32_HOST_OS)
throwSocketErrorIfMinus1RetryMayBlockBut exempt name _ act
  = throwSocketErrorIfMinus1ButRetry exempt name act
#else
throwSocketErrorIfMinus1RetryMayBlockBut _exempt name on_block act =
    throwErrnoIfMinus1RetryMayBlock name act on_block
#endif

{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}

-- ---------------------------------------------------------------------

#if defined(mingw32_HOST_OS)
throwSocketErrorIfMinus1ButRetry :: (Eq a, Num a) =>
                                    (CInt -> Bool) -> String -> IO a -> IO a
throwSocketErrorIfMinus1ButRetry exempt name act = do
  r <- act
  if (r == -1)
   then do
    rc <- c_getLastError
    if rc == wsaNotInitialized then do
        withSocketsDo (return ())
        r' <- act
        if (r' == -1)
           then throwSocketError name
           else return r'
      else
        if (exempt rc)
          then return r
          else throwSocketError name
   else return r

foreign import CALLCONV unsafe "WSAGetLastError"
  c_getLastError :: IO CInt

foreign import ccall unsafe "getWSErrorDescr"
  c_getWSError :: CInt -> IO (Ptr CChar)
#endif

-- ---------------------------------------------------------------------

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.
throwSocketErrorWaitRead :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitRead s name io = withFdSocket s $ \fd ->
    throwSocketErrorIfMinus1RetryMayBlock name
      (threadWaitRead $ fromIntegral fd) io

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be read-ready,
-- and try again.  If it fails with the error the user was expecting then
-- ignore the error
throwSocketErrorWaitReadBut :: (Eq a, Num a) => (CInt -> Bool) -> Socket -> String -> IO a -> IO a
throwSocketErrorWaitReadBut exempt s name io = withFdSocket s $ \fd ->
    throwSocketErrorIfMinus1RetryMayBlockBut exempt name
      (threadWaitRead $ fromIntegral fd) io

-- | Like 'throwSocketErrorIfMinus1Retry', but if the action fails with
-- @EWOULDBLOCK@ or similar, wait for the socket to be write-ready,
-- and try again.
throwSocketErrorWaitWrite :: (Eq a, Num a) => Socket -> String -> IO a -> IO a
throwSocketErrorWaitWrite s name io = withFdSocket s $ \fd ->
    throwSocketErrorIfMinus1RetryMayBlock name
      (threadWaitWrite $ fromIntegral fd) io

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
{-# INLINE withSocketsDo #-}
withSocketsDo :: IO a -> IO a
#if defined(mingw32_HOST_OS)

withSocketsDo act = evaluate withSocketsInit >> act

{-# NOINLINE withSocketsInit #-}
withSocketsInit :: ()
-- Use a CAF to make forcing it do initialisation once, but subsequent forces will be cheap
withSocketsInit = unsafePerformIO $ do
    x <- initWinSock
    when (x /= 0) $ ioError $
      userError "Network.Socket.Internal.withSocketsDo: Failed to initialise WinSock"

foreign import ccall unsafe "initWinSock" initWinSock :: IO Int

#else

withSocketsDo x = x

#endif
