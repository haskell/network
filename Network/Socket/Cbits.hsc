module Network.Socket.Cbits where

#include "HsNet.h"

import Network.Socket.Imports

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = #const SOMAXCONN

#if defined(mingw32_HOST_OS)
wsaNotInitialized :: CInt
wsaNotInitialized = #const WSANOTINITIALISED
#endif
