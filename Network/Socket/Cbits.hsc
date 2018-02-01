module Network.Socket.Cbits where

#include "HsNet.h"

import Network.Socket.Imports

#if defined(mingw32_HOST_OS)
wsaNotInitialized :: CInt
wsaNotInitialized = #const WSANOTINITIALISED
#endif
