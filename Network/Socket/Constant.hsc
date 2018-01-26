#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Constant where

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = #const SOMAXCONN
