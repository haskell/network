{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Network.Socket.ByteString
-- Copyright   : (c) Johan Tibell 2007
-- License     : BSD-style
--
-- Maintainer  : johan.tibell@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- A module for efficiently transmitting data over sockets. For detailed
-- documentation, consult your favorite POSIX socket reference. All functions
-- communicate failures by converting the error number to 'System.IO.IOError'.
--
-- This module is made to be imported with 'Network.Socket' like so:
--
-- > import Network.Socket hiding (send, sendTo, recv, recvFrom)
-- > import Network.Socket.ByteString
--
module Network.Socket.ByteString
  ( -- * Send messages on sockets
    -- | Functions for sending messages on sockets
    send
  , sendAll
#if !defined(mingw32_HOST_OS)
  , sendMany
#endif
  , sendTo
  , sendAllTo
#if !defined(mingw32_HOST_OS)
  , sendManyTo
#endif

    -- * Receive messages from sockets
    -- | Functions for receiving messages from sockets
  , recv
  , recvFrom

    -- * Example
    -- $example
  ) where

import Control.Monad (liftM, when)
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Network.Socket (SockAddr, Socket(..), sendBufTo, recvBufFrom)
import Network.Socket.ByteString.Internal

#if !defined(mingw32_HOST_OS)
import Control.Monad (zipWithM_)
import Data.List (iterate)
import Foreign.C.Types (CChar, CSize)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Network.Socket.ByteString.IOVec
import Network.Socket.ByteString.MsgHdr
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock,
                                withSockAddr)

#  if defined(__GLASGOW_HASKELL__)
import GHC.Conc (threadWaitRead, threadWaitWrite)
#  endif
#else
#  if defined(__GLASGOW_HASKELL__)
import GHC.Handle (readRawBufferPtr, writeRawBufferPtr)
#  endif
#endif

#if defined(HAVE_WINSOCK_H) && !defined(cygwin32_HOST_OS)
#  define WITH_WINSOCK 1
#endif

#if !defined(CALLCONV)
#  ifdef WITH_WINSOCK
#    define CALLCONV stdcall
#  else
#    define CALLCONV ccall
#  endif
#endif

#if !defined(mingw32_HOST_OS)
foreign import CALLCONV unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
foreign import CALLCONV unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
#endif

-- -----------------------------------------------------------------------------
-- Sending

-- | Send a message on a socket. The socket must be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for ensuring
-- that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Number of bytes sent
send (MkSocket s _ _ _ _) xs =
    unsafeUseAsCStringLen xs $ \(str, len) ->
    liftM fromIntegral $
#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)
        writeRawBufferPtr "Network.Socket.ByteString.send"
        (fromIntegral s) True str 0 (fromIntegral len)
#else
#  if !defined(__HUGS__)
        throwSocketErrorIfMinus1RetryMayBlock "send"
        (threadWaitWrite (fromIntegral s)) $
#  endif
        c_send s str (fromIntegral len) 0
#endif

-- | Send a message on a socket. The socket must be in a connected state. This
-- function continues to send data until either all data has been sent or an
-- error occurs. If there is an error, an exception is raised, and there is no
-- way to determine how much data was sent.
sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

#if !defined(mingw32_HOST_OS)
-- | Send a multi-part message on a socket. The socket must be in a connected
-- state. The data is sent as if the parts have been concatenated. This
-- function continues to send data until either all data has been sent or an
-- error occurs. If there is an error, an exception is raised, and there is no
-- way to determine how much data was sent.  /Unix only/.
sendMany :: Socket        -- ^ Connected socket
         -> [ByteString]  -- ^ Data to send
         -> IO ()
sendMany sock@(MkSocket fd _ _ _ _) cs = do
    sent <- sendManyInner
    when (sent < totalLength cs) $ sendMany sock (remainingChunks sent cs)
  where
    sendManyInner =
      liftM fromIntegral . withIOVec cs $ \(iovsPtr, iovsLen) ->
          throwSocketErrorIfMinus1RetryMayBlock "writev"
              (threadWaitWrite (fromIntegral fd)) $
              c_writev (fromIntegral fd) iovsPtr (fromIntegral iovsLen)
#endif

-- | Send a message on a socket. The recipient can be specified explicitly, so
-- the socket need not be in a connected state. Returns the number of bytes
-- sent. Applications are responsible for ensuring that all data has been sent.
sendTo :: Socket      -- ^ Socket
       -> ByteString  -- ^ Data to send
       -> SockAddr    -- ^ Recipient address
       -> IO Int      -- ^ Number of bytes sent
sendTo sock xs addr =
    unsafeUseAsCStringLen xs $ \(str, len) -> sendBufTo sock str len addr

-- | Send a message on a socket. The recipient can be specified explicitly, so
-- the socket need not be in a connected state. This function continues to send
-- data until either all data has been sent or an error occurs. If there is an
-- error, an exception is raised, and there is no way to determine how much
-- data was sent.
sendAllTo :: Socket      -- ^ Socket
          -> ByteString  -- ^ Data to send
          -> SockAddr    -- ^ Recipient address
          -> IO ()
sendAllTo sock xs addr = do
    sent <- sendTo sock xs addr
    when (sent < B.length xs) $ sendAllTo sock (B.drop sent xs) addr

#if !defined(mingw32_HOST_OS)
-- | Send a multi-part message on a socket. The recipient can be specified
-- explicitly, so the socket need not be in a connected state. The data is sent
-- as if the parts have been concatenated. This function continues to send data
-- until either all data has been sent or an error occurs. If there is an
-- error, an exception is raised, and there is no way to determine how much
-- data was sent.  /Unix only/.
sendManyTo :: Socket        -- ^ Socket
           -> [ByteString]  -- ^ Data to send
           -> SockAddr      -- ^ Recipient address
           -> IO ()
sendManyTo sock@(MkSocket fd _ _ _ _) cs addr = do
    sent <- liftM fromIntegral sendManyToInner
    when (sent < totalLength cs) $ sendManyTo sock (remainingChunks sent cs) addr
  where
    sendManyToInner =
      withSockAddr addr $ \addrPtr addrSize ->
        withIOVec cs $ \(iovsPtr, iovsLen) -> do
          let msgHdr = MsgHdr
                addrPtr (fromIntegral addrSize)
                iovsPtr (fromIntegral iovsLen)
                nullPtr 0
                0
          with msgHdr $ \msgHdrPtr ->
            throwSocketErrorIfMinus1RetryMayBlock "sendmsg"
              (threadWaitWrite (fromIntegral fd)) $
              c_sendmsg (fromIntegral fd) msgHdrPtr 0
#endif

-- -----------------------------------------------------------------------------
-- Receiving

-- | Receive a message from a socket. The socket must be in a connected state.
-- This function may return fewer bytes than specified. If the message is
-- longer than the specified length, it may be discarded depending on the type
-- of socket. This function may block until a message arrives.
--
-- Considering hardware and network realities, the maximum number of bytes to
-- receive should be a small power of 2, e.g., 4096.
--
-- For TCP sockets, a zero length return value means the peer has
-- closed its half side of the connection.
recv :: Socket         -- ^ Connected socket
     -> Int            -- ^ Maximum number of bytes to receive
     -> IO ByteString  -- ^ Data received
recv (MkSocket s _ _ _ _) nbytes
    | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.ByteString.recv")
    | otherwise   = createAndTrim nbytes $ recvInner s nbytes

recvInner :: CInt -> Int -> Ptr Word8 -> IO Int
recvInner s nbytes ptr =
    fmap fromIntegral $
#if defined(__GLASGOW_HASKELL__) && defined(mingw32_HOST_OS)
        readRawBufferPtr "Network.Socket.ByteString.recv" (fromIntegral s)
        True (castPtr ptr) 0 (fromIntegral nbytes)
#else
#  if !defined(__HUGS__)
        throwSocketErrorIfMinus1RetryMayBlock "recv"
        (threadWaitRead (fromIntegral s)) $
#  endif
        c_recv s (castPtr ptr) (fromIntegral nbytes) 0
#endif

-- | Receive a message from a socket. The socket need not be in a connected
-- state. Returns @(bytes, address)@ where @bytes@ is a 'ByteString'
-- representing the data received and @address@ is a 'SockAddr' representing
-- the address of the sending socket.
recvFrom :: Socket                     -- ^ Socket
         -> Int                        -- ^ Maximum number of bytes to receive
         -> IO (ByteString, SockAddr)  -- ^ Data received and sender address
recvFrom sock nbytes =
    allocaBytes nbytes $ \ptr -> do
        (len, sockaddr) <- recvBufFrom sock ptr nbytes
        str <- B.packCStringLen (ptr, len)
        return (str, sockaddr)

-- -----------------------------------------------------------------------------
-- Not exported

#if !defined(mingw32_HOST_OS)
-- | Suppose we try to transmit a list of chunks @cs@ via a gathering write
-- operation and find that @n@ bytes were sent. Then @remainingChunks n cs@ is
-- list of chunks remaining to be sent.
remainingChunks :: Int -> [ByteString] -> [ByteString]
remainingChunks _ [] = []
remainingChunks i (x:xs)
    | i < len        = B.drop i x : xs
    | otherwise      = let i' = i - len in i' `seq` remainingChunks i' xs
  where
    len = B.length x

-- | @totalLength cs@ is the sum of the lengths of the chunks in the list @cs@.
totalLength :: [ByteString] -> Int
totalLength = sum . map B.length

-- | @withIOVec cs f@ executes the computation @f@, passing as argument a pair
-- consisting of a pointer to a temporarily allocated array of pointers to
-- 'IOVec' made from @cs@ and the number of pointers (@length cs@).
-- /Unix only/.
withIOVec :: [ByteString] -> ((Ptr IOVec, Int) -> IO a) -> IO a
withIOVec cs f =
    allocaArray csLen $ \aPtr -> do
        zipWithM_ pokeIov (ptrs aPtr) cs
        f (aPtr, csLen)
  where
    csLen = length cs
    ptrs = iterate (`plusPtr` sizeOf (undefined :: IOVec))
    pokeIov ptr s =
        unsafeUseAsCStringLen s $ \(sPtr, sLen) ->
        poke ptr $ IOVec sPtr (fromIntegral sLen)
#endif

-- ---------------------------------------------------------------------
-- Example

-- $example
--
-- Here are two minimal example programs using the TCP/IP protocol: a
-- server that echoes all data that it receives back (servicing only
-- one client) and a client using it.
--
-- > -- Echo server program
-- > module Main where
-- >
-- > import Control.Monad
-- > import qualified Data.ByteString as S
-- > import Network.Socket hiding (recv)
-- > import Network.Socket.ByteString
-- >
-- > main :: IO ()
-- > main = withSocketsDo $
-- >     do addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
-- >        let serveraddr = head addrinfos
-- >        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
-- >        bindSocket sock (addrAddress serveraddr)
-- >        listen sock 1
-- >        (conn, _) <- accept sock
-- >        talk conn
-- >        sClose conn
-- >        sClose sock
-- >
-- >     where
-- >       talk :: Socket -> IO ()
-- >       talk conn =
-- >           do msg <- recv conn 1024
-- >              unless (S.null msg) $ sendAll conn msg >> talk conn
--
-- > -- Echo client program
-- > module Main where
-- >
-- > import qualified Data.ByteString.Char8 as C
-- > import Network.Socket hiding (recv)
-- > import Network.Socket.ByteString
-- >
-- > main :: IO ()
-- > main = withSocketsDo $
-- >     do addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
-- >        let serveraddr = head addrinfos
-- >        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
-- >        connect sock (addrAddress serveraddr)
-- >        sendAll sock $ C.pack "Hello, world!"
-- >        msg <- recv sock 1024
-- >        sClose sock
-- >        putStr "Received "
-- >        C.putStrLn msg
