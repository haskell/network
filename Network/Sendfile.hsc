{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Sendfile
-- Copyright   :  (c) Volker Stolz 2003 <vs@foldr.org>
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  provides fallback
--
-- 'Network.Sendfile.sendfile' is a low-level method for efficently passing
-- data from one file descriptor to another.
-- The intended audience includes for example web server authors. Please
-- note that this function is highly platform dependent.
--
-----------------------------------------------------------------------------

module Network.Sendfile (
  -- * Haskell wrappers
  sendfile,
  sendfileByName,
  -- * Fallback implementation
  squirt

) where

#include "HsNet.h"

import Foreign
import Foreign.C
import Network.Socket
import System.IO
import System.Posix.Types	( Fd, Fd(..), COff )
import Control.Exception	( bracket )
import Control.Monad		( unless)
import Data.Array.MArray
import Data.Array.IO
#ifdef __GLASGOW_HASKELL__
import GHC.Handle
import GHC.IOBase
#endif

-- |'sendfile' transmits the contents of an open file to a stream socket
-- opened for writing with as little overhead as possible.
-- This function is not defined by any standard! Passing '0' will indeed
-- transmit nothing at all.
-- Caveats for 'handleToFd' apply.

sendfile :: Fd		-- ^ Input
         -> Socket	-- ^ Output
         -> Int		-- ^ Offset
         -> Int		-- ^ Nr. of bytes to transmit
         -> IO ()

sendfile _inFd _outS _startpos 0 = return ()
sendfile inFd outS startpos count = do
  outH  <- socketToHandle outS WriteMode
  outFd <- handleToFd outH

#if defined(HAVE_LINUX_SENDFILE) && !defined(__USE_FILE_OFFSET64)
  offsetptr <- malloc
  poke offsetptr (fromIntegral startpos)
  throwErrnoIfMinus1_ "sendfile" $ c_sendfile outFd inFd offsetptr (fromIntegral count)
  free offsetptr
  return ()

foreign import ccall unsafe "sendfile"
  c_sendfile :: Fd -> Fd -> Ptr COff -> CSize -> IO CSize

#else /* Linux */
# ifdef HAVE_BSD_SENDFILE
  offsetptr <- malloc
  rc <- sendfileLoop inFd outFd (fromIntegral startpos) (fromIntegral count) offsetptr
  free offsetptr
  return ()
 where
  sendfileLoop :: Fd -> Fd -> COff -> CSize -> Ptr COff -> IO CInt
  sendfileLoop inFd outFd start c offsetptr = do
    rc <- c_sendfile inFd outFd start c nullPtr offsetptr 0
    if rc == -1
     then do
       err <- getErrno
       if err == eAGAIN 
          then do offset <- peek offsetptr -- now contains # of bytes written
                  sendfileLoop inFd outFd (start+offset)  (c-(fromIntegral offset)) offsetptr
          else throwErrno "sendfile"
     else 
       return rc

foreign import ccall unsafe "sendfile"
  c_sendfile :: Fd -> Fd -> COff -> CSize -> Ptr a -> Ptr COff -> CInt -> IO CInt

# else /* BSD */
  inH <- fdToHandle inFd
  squirt inH outH startpos count
# endif /* no native */
#endif

-- |'sendfileByName' sends a file to an already open descriptor
-- using 'sendfile'. You can only use this function on regular
-- files.
sendfileByName :: String -> Socket -> IO ()
sendfileByName filename outS = do
  bracket 
    (openFile filename ReadMode)
    (\handle -> hClose handle)
    (\handle -> do
        size <- hFileSize handle
	inFd <- handleToFd handle
	sendfile inFd outS 0 (fromIntegral size))
  
-- squirt data from 'rd' into 'wr' as fast as possible.  We use a 4k
-- single buffer. Stolen from Simon M.'s Haskell Web Server fptools/hws
-- We have to revert the handleToFd.

-- |Fallback API. Exported in case somebody needs it.

squirt  :: Handle	-- ^ Input
         -> Handle	-- ^ Output
         -> Int		-- ^ Offset
         -> Int		-- ^ Nr. of bytes to transmit
         -> IO ()
squirt inH outH startpos count = do
  hSeek inH RelativeSeek (fromIntegral startpos)
  arr <- Data.Array.MArray.newArray_ (0, bufsize-1)
  let loop remaining = do
        r <- hGetArray inH arr (min bufsize remaining)
	unless (r == 0) $
	  do if (r < bufsize) 
     		then hPutArray outH arr r
     		else hPutArray outH arr bufsize >> loop (remaining-bufsize)
  loop count
  hFlush outH

bufsize = 4 * 1024 :: Int

-- Stolen from System.Posix.IO:

#ifdef __GLASGOW_HASKELL__
handleToFd :: Handle -> IO Fd
handleToFd h = withHandle "handleToFd" h $ \ h_ -> do
  -- converting a Handle into an Fd effectively means
  -- letting go of the Handle; it is put into a closed
  -- state as a result.
  let fd = haFD h_  
  flushWriteBufferOnly h_
  unlockFile (fromIntegral fd)
    -- setting the Handle's fd to (-1) as well as its 'type'
    -- to closed, is enough to disable the finalizer that
    -- eventually is run on the Handle.
  return (h_{haFD= (-1),haType=ClosedHandle}, Fd (fromIntegral fd))
#endif
