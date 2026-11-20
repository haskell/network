{-# LANGUAGE CPP #-}

#include "HsNetDef.h"

-- | The Windows side of "Network.Socket.Internal": errors are Winsock
--   codes from @WSAGetLastError@, and readiness is waited for through an
--   AFD poll.
module Network.Socket.Win32.Internal (
    throwSocketError,
    throwSocketErrorCode,
    throwSocketErrorIfMinus1_,
    throwSocketErrorIfMinus1Retry,
    throwSocketErrorIfMinus1RetryMayBlock,
    throwSocketErrorIfMinus1RetryMayBlockBut,
    waitReadSocket,
    waitWriteSocket,
    withSocketsDo,
    -- * Windows only
    c_getLastError,
    throwSocketErrorIfMinus1ButRetry,
    winsockErrorString,
    withOverlappedSocket,
) where

import qualified Control.Exception as E
import qualified GHC.Event.Windows as Mgr
import GHC.IO.Exception (IOException (..))
import GHC.IO.SubSystem (IoSubSystem (..), ioSubSystem)
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.IO.Unsafe (unsafePerformIO)

import Network.Socket.Cbits
import Network.Socket.Imports
import Network.Socket.Types
import Network.Socket.Win32.AfdPoll

throwSocketError :: String -> IO a
throwSocketError name = c_getLastError >>= throwSocketErrorCode name

-- The code is a Winsock code rather than an errno.  It is kept in
-- 'ioe_errno' all the same, and the error type is the one the errno of
-- the same name has elsewhere.
throwSocketErrorCode :: String -> CInt -> IO a
throwSocketErrorCode name rc = do
    str <- winsockErrorString rc
    let typ = winsockErrorType rc
        ioe = (mkIOError typ name Nothing Nothing){ioe_errno = Just rc}
    ioError (ioeSetErrorString ioe str)

throwSocketErrorIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwSocketErrorIfMinus1_ name act = void $ throwSocketErrorIfMinus1Retry name act
{-# SPECIALIZE throwSocketErrorIfMinus1_ :: String -> IO CInt -> IO () #-}

throwSocketErrorIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwSocketErrorIfMinus1Retry = throwSocketErrorIfMinus1ButRetry (const False)
{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> IO CInt -> IO CInt #-}

throwSocketErrorIfMinus1RetryMayBlock
    :: (Eq a, Num a) => String -> IO b -> IO a -> IO a
throwSocketErrorIfMinus1RetryMayBlock =
    throwSocketErrorIfMinus1RetryMayBlockBut (const False)
{-# SPECIALIZE throwSocketErrorIfMinus1RetryMayBlock
        :: String -> IO b -> IO CInt -> IO CInt #-}

throwSocketErrorIfMinus1RetryMayBlockBut
    :: (Eq a, Num a) => (CInt -> Bool) -> String -> IO b -> IO a -> IO a
throwSocketErrorIfMinus1RetryMayBlockBut exempt name on_block act = loop
  where
    loop = do
        r <- throwSocketErrorIfMinus1ButRetry exempt' name act
        if r /= -1 then return r else do
            rc <- c_getLastError
            if rc == wsaEWOULDBLOCK then on_block >> loop else return r
    exempt' rc = rc == wsaEWOULDBLOCK || exempt rc

-- | Throw the current Winsock error if the action returns @-1@, unless
--   the predicate exempts its code: then return @-1@.  If Winsock was
--   not initialized, initialize it and run the action once more.
throwSocketErrorIfMinus1ButRetry
    :: (Eq a, Num a) => (CInt -> Bool) -> String -> IO a -> IO a
throwSocketErrorIfMinus1ButRetry exempt name act = do
    r <- act
    if r == -1
        then do
            rc <- c_getLastError
            if rc == wsaNotInitialized
                then do
                    withSocketsDo (return ())
                    r' <- act
                    if r' == -1 then throwSocketError name else return r'
                else
                    if exempt rc then return r else throwSocketError name
        else return r

-- | The current Winsock error code (@WSAGetLastError@).
foreign import CALLCONV unsafe "WSAGetLastError"
  c_getLastError :: IO CInt

foreign import ccall unsafe "getWSErrorDescr"
  c_getWSError :: CInt -> IO (Ptr CChar)

-- | A description of a Winsock error code.
winsockErrorString :: CInt -> IO String
winsockErrorString rc = do
    pstr <- c_getWSError rc
    if pstr == nullPtr
        then return $ "Unknown WinSock error: " ++ show rc
        else peekCString pstr

-- | Running an overlapped operation on the socket, which the start action
--   begins and reports as @WSASend@ and friends do.  A failure comes back
--   as a value rather than thrown by the I/O manager, which knows neither
--   NTSTATUS nor Winsock codes, and is raised as other socket errors are,
--   unless the last argument turns it into a result.
withOverlappedSocket
    :: String
    -> CSocket
    -> (Mgr.LPOVERLAPPED -> IO CInt)
    -> (CInt -> Maybe Int)
    -> IO Int
withOverlappedSocket name fd start result = do
    r <- Mgr.withOverlapped name (wordPtrToPtr $ fromIntegral fd) 0 startCB completionCB
    case r of
        Mgr.IOSuccess (Right n) -> return n
        Mgr.IOSuccess (Left rc) -> throwSocketErrorCode name rc
        Mgr.IOFailed e -> throwSocketErrorCode name (maybe 0 fromIntegral e)
  where
    -- An overlapped socket queues a completion packet even when the call
    -- succeeds at once, so the I/O manager resolves that case too.
    startCB ov = do
        ret <- start ov
        if ret == 0 then return Mgr.CbPending else do
            rc <- c_getLastError
            return $ if rc == errorIoPending
                then Mgr.CbPending
                else Mgr.CbError (fromIntegral rc)
    completionCB st bytes = Mgr.ioSuccess $ case c_winsock_code st of
        0 -> Right $ fromIntegral bytes
        rc -> maybe (Left rc) Right $ result rc
    errorIoPending = 997

-- The completion status as a Winsock code: an NTSTATUS when the operation
-- completed through the port, a Winsock code when it failed at once.
foreign import ccall unsafe "hsnet_winsock_code"
  c_winsock_code :: Word32 -> CInt

waitReadSocket, waitWriteSocket :: Socket -> CSocket -> IO ()
waitReadSocket s _ = afdWait "waitReadSocket" afdRead s
waitWriteSocket s _ = afdWait "waitWriteSocket" afdWrite s

withSocketsDo :: IO a -> IO a
withSocketsDo act = E.evaluate withSocketsInit >> act
{-# INLINE withSocketsDo #-}

-- A CAF, so that forcing it initialises Winsock once and is cheap after.
{-# NOINLINE withSocketsInit #-}
withSocketsInit :: ()
withSocketsInit = unsafePerformIO $ do
    -- The I/O manager is the executable's choice, made at start-up.
    case ioSubSystem of
        IoNative -> return ()
        IoPOSIX -> ioError $ userError $
            "Network.Socket: network needs the WinIO I/O manager on Windows: "
                ++ "run with +RTS --io-manager=native -RTS, or link with "
                ++ "-with-rtsopts=--io-manager=native"
    x <- initWinSock
    when (x /= 0) $ ioError $
      userError "Network.Socket.Internal.withSocketsDo: Failed to initialise WinSock"

foreign import ccall unsafe "initWinSock" initWinSock :: IO Int
