#include "HsNet.h"

module Network.Socket.ByteString.Flag where

import Network.Socket.Imports
import Network.Socket.Info

-- | Message flags.
data MsgFlag =
    MSG_OOB        -- ^ Send or receive OOB(out-of-bound) data.
  | MSG_DONTROUTE  -- ^ Bypass routing table lookup.
  | MSG_PEEK       -- ^ Peek at incoming message without removing it from the queue.
  | MSG_EOR        -- ^ End of record.
  | MSG_TRUNC      -- ^ Received data is truncated. More data exist.
  | MSG_CTRUNC     -- ^ Received control message is truncated. More control message exist.
  | MSG_WAITALL    -- ^ Wait until the requested number of bytes have been read.
  deriving (Eq, Show)

msgFlagMapping :: [(MsgFlag, CInt)]
msgFlagMapping = [
#ifdef MSG_OOB
    (MSG_OOB, #const MSG_OOB)
#else
    (MSG_OOB, 0)
#endif
#ifdef MSG_DONTROUTE
  , (MSG_DONTROUTE, #const MSG_DONTROUTE)
#else
  , (MSG_DONTROUTE, 0)
#endif
#ifdef MSG_PEEK
  , (MSG_PEEK, #const MSG_PEEK)
#else
  , (MSG_PEEK, 0)
#endif
#ifdef MSG_EOR
  , (MSG_EOR, #const MSG_EOR)
#else
  , (MSG_EOR, 0)
#endif
#ifdef MSG_TRUNC
  , (MSG_TRUNC, #const MSG_TRUNC)
#else
  , (MSG_TRUNC, 0)
#endif
#ifdef MSG_CTRUNC
  , (MSG_CTRUNC, #const MSG_CTRUNC)
#else
  , (MSG_CTRUNC, 0)
#endif
#ifdef MSG_WAITALL
  , (MSG_WAITALL, #const MSG_WAITALL)
#else
  , (MSG_WAITALL, 0)
#endif
  ]

msgFlagImplemented :: MsgFlag -> Bool
msgFlagImplemented f = packBits msgFlagMapping [f] /= 0
