-- | The POSIX side of "Network.Socket.Unix".
module Network.Socket.Posix.Unix (
    socketPair,
) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)

import Network.Socket.Fcntl
import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types

socketPair :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)
socketPair family stype protocol =
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
      let c_stype = packSocketType stype
      _rc <- throwSocketErrorIfMinus1Retry "Network.Socket.socketpair" $
                  c_socketpair (packFamily family) c_stype protocol fdArr
      [fd1,fd2] <- peekArray 2 fdArr
      setNonBlockIfNeeded fd1
      setNonBlockIfNeeded fd2
      s1 <- mkSocket fd1
      s2 <- mkSocket fd2
      return (s1, s2)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt
