{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS)
#  include "windows.h"
#endif

module Network.Socket.Win32.WinIO where

import Data.Word
import Foreign.Ptr
import System.Win32.Types

#if defined(__IO_MANAGER_WINIO__)
import GHC.Event.Windows (withOverlapped, getSystemManager)
import GHC.IO.SubSystem ((<!>))
#endif

withAsyncSupport :: String -> HANDLE -> Word64 -> Ptr b -> (HANDLE -> Ptr b -> Ptr () -> IO a) -> IO a
withAsyncSupport _label hwnd _offset buffptr action = do
#if !defined(__IO_MANAGER_WINIO__)
  action hwnd (castPtr buffptr) nullPtr
#else
  case ioSubSystem of
    IoPOSIX -> action hwnd (castPtr buffptr) nullPtr
    _  -> do
      mngr <- Mgr.getSystemManager
      fmap fromIntegral $ Mgr.withException label $
         withOverlappedEx mngr _label hwnd True
                          _offset (startCB buffptr) completionCB
      where
        startCB outBuf lpOverlapped = do
          ret <- action hwnd (castPtr outBuf) lpOverlapped
          return $ Mgr.CbNone ret

        completionCB err dwBytes
          | err == #{const ERROR_SUCCESS}       = Mgr.ioSuccess $ fromIntegral dwBytes
          | err == #{const ERROR_HANDLE_EOF}    = Mgr.ioSuccess 0
          | err == #{const STATUS_END_OF_FILE}  = Mgr.ioSuccess 0
          | err == #{const ERROR_BROKEN_PIPE}   = Mgr.ioSuccess 0
          | err == #{const STATUS_PIPE_BROKEN}  = Mgr.ioSuccess 0
          | err == #{const ERROR_NO_MORE_ITEMS} = Mgr.ioSuccess $ fromIntegral dwBytes
          | err == #{const ERROR_MORE_DATA}     = Mgr.ioSuccess $ fromIntegral dwBytes
          | otherwise                           = Mgr.ioFailed err
#endif