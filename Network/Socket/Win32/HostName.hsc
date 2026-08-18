{-# LANGUAGE CPP #-}

#include "HsNet.h"
##include "HsNetDef.h"

module Network.Socket.Win32.HostName (
    getHostName,
)
where

import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Network.Socket.Info (HostName)
import System.Win32.Types

foreign import CALLCONV unsafe "windows.h GetComputerNameExW"
    getComputerNameEx :: COMPUTER_NAME_FORMAT -> LPTSTR -> LPDWORD -> IO BOOL

type COMPUTER_NAME_FORMAT = CInt

computerNamePhysicalDnsHostname :: COMPUTER_NAME_FORMAT
computerNamePhysicalDnsHostname = 5

-- | Get name of current host
--
--   Since: 3.3.0.0
getHostName :: IO HostName
getHostName = with 0 $ \p_charcount -> do
    -- On the first run, determine the character count and ignore any error we get
    _ <- getComputerNameEx computerNamePhysicalDnsHostname nullPtr p_charcount
    charcount <- peek p_charcount

    -- The second time around, use the correct character count to retrieve the data
    withTString (replicate (fromIntegral charcount) ' ') $ \name -> do
        failIfFalse_ "GetComputerNameExW" $
            getComputerNameEx computerNamePhysicalDnsHostname name p_charcount
        peekTString name
