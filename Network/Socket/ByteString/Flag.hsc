{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

#include "HsNet.h"

module Network.Socket.ByteString.Flag where

import Network.Socket.Imports

-- | Message flags. To combine flags, use '(<>)'.
newtype MsgFlag = MsgFlag { fromMsgFlag :: CInt }
                deriving (Show, Eq, Ord, Num, Bits)

instance Semigroup MsgFlag where
  (<>) = (.|.)

instance Monoid MsgFlag where
  mempty = 0

-- | Send or receive OOB(out-of-bound) data.
pattern MSG_OOB :: MsgFlag
#ifdef MSG_OOB
pattern MSG_OOB = MsgFlag (#const MSG_OOB)
#else
pattern MSG_OOB = mempty
#endif

-- | Bypass routing table lookup.
pattern MSG_DONTROUTE :: MsgFlag
#ifdef MSG_DONTROUTE
pattern MSG_DONTROUTE = MsgFlag (#const MSG_DONTROUTE)
#else
pattern MSG_DONTROUTE = mempty
#endif

-- | Peek at incoming message without removing it from the queue.
pattern MSG_PEEK :: MsgFlag
#ifdef MSG_PEEK
pattern MSG_PEEK = MsgFlag (#const MSG_PEEK)
#else
pattern MSG_PEEK = mempty
#endif

-- | End of record.
pattern MSG_EOR :: MsgFlag
#ifdef MSG_EOR
pattern MSG_EOR = MsgFlag (#const MSG_EOR)
#else
pattern MSG_EOR = mempty
#endif

-- | Received data is truncated. More data exist.
pattern MSG_TRUNC :: MsgFlag
#ifdef MSG_TRUNC
pattern MSG_TRUNC = MsgFlag (#const MSG_TRUNC)
#else
pattern MSG_TRUNC = mempty
#endif

-- | Received control message is truncated. More control message exist.
pattern MSG_CTRUNC :: MsgFlag
#ifdef MSG_CTRUNC
pattern MSG_CTRUNC = MsgFlag (#const MSG_CTRUNC)
#else
pattern MSG_CTRUNC = mempty
#endif

-- | Wait until the requested number of bytes have been read.
pattern MSG_WAITALL :: MsgFlag
#ifdef MSG_WAITALL
pattern MSG_WAITALL = MsgFlag (#const MSG_WAITALL)
#else
pattern MSG_WAITALL = mempty
#endif
