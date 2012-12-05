{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -Wall #-}
module Network.Socket.WinSelect (
    select1,
    select1NoBlock,
    select1WithTimeout,

    -- * Event
    Event,
    evtRead,
    evtWrite,
    evtExcept,

    -- ** Set manipulation
    (<>),
    mempty,
    testEvent,
) where

#include "HsNet.h"

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Data.Bits
import Data.IORef
import Data.Monoid
import Foreign
import Foreign.C
import GHC.Conc.Windows     (asyncDoProc)

-- | Wait for socket readiness using the @select()@ function.  The supplied
-- event mask may not be empty, and 'select1' will never return an empty
-- event mask.
--
-- This returns a Winsock error code if @select()@ fails, rather than throwing
-- an exception.  This avoids a dependency cycle with the
-- "Network.Socket.Internal" module.
--
-- 'select1' can be interrupted with an asynchronous exception.
select1 :: CInt -> Event -> IO (Either CInt Event)
select1 = select1' timeoutIndefinite

-- | Like 'select1', but times out and returns @'Right' 'mempty'@ if none of
-- the selected events occur after the given number of milliseconds.
select1WithTimeout :: CInt  -- ^ Socket descriptor
                   -> Event -- ^ Events to wait for (may not be empty)
                   -> Int   -- ^ Timeout in milliseconds.  If <= 0,
                            --   this will do a non-blocking @select()@.
                   -> IO (Either CInt Event)
select1WithTimeout fd ev msecs
    | msecs <= 0 = select1NoBlock fd ev
    | otherwise  = select1' (timeoutMSecs msecs) fd ev

select1' :: TimeoutStep -> CInt -> Event -> IO (Either CInt Event)
select1' ts0 sock ev =
    mask_ $ do
        mv <- newEmptyMVar :: IO (MVar (Either CInt Event))
        canceledRef <- newIORef False

        let loop (TimeoutNext tv_sec tv_usec ts) = do
                res <- select1Raw sock ev tv_sec tv_usec
                case res of
                    Right (Event 0) -> do
                        canceled <- readIORef canceledRef
                        if canceled
                            then return ()
                            else loop (ts ())
                    _ -> putMVar mv res

            loop TimeoutDone =
                putMVar mv $ Right $ Event 0

        _ <- forkIO $ loop ts0

        takeMVar mv `onException` set canceledRef True

  where
    set ref x = atomicModifyIORef ref (\_ -> (x, ()))

-- | Perform a blocking @select()@ call, using 'asyncDoProc' under the
-- non-threaded RTS.  'select1Raw' must not be interrupted with an
-- asynchronous exception.
select1Raw :: CInt -> Event -> CLong -> CLong -> IO (Either CInt Event)
select1Raw sock ev tv_sec tv_usec =
    withSelect1Data $ \sd -> do
        c_network_select1InitTimeout sd (fromIntegral sock) ev tv_sec tv_usec
        if threaded
            then c_network_select1 sd
            else asyncDoProc c_network_select1_fp sd

-- | Non-blocking version of 'select1'.  Returns 'mempty' if the socket is not
-- ready on any of the events requested.  Like 'select1', the supplied event
-- mask may not be empty.
select1NoBlock :: CInt -> Event -> IO (Either CInt Event)
select1NoBlock sock ev =
    withSelect1Data $ \sd -> do
        c_network_select1InitTimeout sd (fromIntegral sock) ev 0 0
        c_network_select1_unsafe sd

------------------------------------------------------------------------
-- select() timeout generators

data TimeoutStep = TimeoutNext !CLong !CLong !(() -> TimeoutStep)
                 | TimeoutDone

timeoutIndefinite :: TimeoutStep
timeoutIndefinite = TimeoutNext 60 0 (\_ -> timeoutIndefinite)

timeoutMSecs :: Int -> TimeoutStep
timeoutMSecs msecs
    | msecs > 60000 = TimeoutNext 60 0 (\_ -> timeoutMSecs (msecs - 60000))
    | otherwise     = let (s, m) = fromIntegral msecs `divMod` 1000
                       in TimeoutNext s m (\_ -> TimeoutDone)

------------------------------------------------------------------------
-- Raw bindings

type SOCKET = #type SOCKET

data Select1Data

withSelect1Data :: (Ptr Select1Data -> IO Int) -> IO (Either CInt Event)
withSelect1Data cb =
    allocaSelect1Data $ \sd -> do
        rc <- cb sd
        if rc < 0
            then Left  . fromIntegral <$> peekLastError sd
            else Right . Event        <$> peekEvtMask   sd

allocaSelect1Data :: (Ptr Select1Data -> IO a) -> IO a
allocaSelect1Data = allocaBytes #{size Select1Data}

peekEvtMask :: Ptr Select1Data -> IO CInt
peekEvtMask = #{peek Select1Data, evtMask}

peekLastError :: Ptr Select1Data -> IO CInt
peekLastError = #{peek Select1Data, lastError}

-- foreign import ccall unsafe "HsNet.h c_network_select1Init"
--     c_network_select1Init :: Ptr Select1Data -> SOCKET -> Event -> IO ()

foreign import ccall unsafe "HsNet.h c_network_select1InitTimeout"
    c_network_select1InitTimeout
        :: Ptr Select1Data
        -> SOCKET
        -> Event
        -> CLong    -- ^ tv_sec
        -> CLong    -- ^ tv_usec
        -> IO ()

foreign import ccall safe "HsNet.h c_network_select1"
    c_network_select1 :: Ptr Select1Data -> IO Int

-- Use with timeout of {0, 0} for non-blocking status check.
foreign import ccall unsafe "HsNet.h c_network_select1"
    c_network_select1_unsafe :: Ptr Select1Data -> IO Int

-- Use with asyncDoProc
foreign import ccall "HsNet &c_network_select1"
    c_network_select1_fp :: FunPtr (Ptr Select1Data -> IO Int)

foreign import ccall unsafe "rtsSupportsBoundThreads"
    threaded :: Bool

------------------------------------------------------------------------
-- Event

newtype Event = Event CInt
    deriving Eq

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

instance Monoid Event where
    mempty = Event 0
    mappend (Event a) (Event b) = Event (a .|. b)

instance Show Event where
    showsPrec d ev =
        combine ( test evtRead   "evtRead"
                . test evtWrite  "evtWrite"
                . test evtExcept "evtExcept"
                )
      where
        test b name xs | testEvent b ev = name : xs
                       | otherwise      = xs

        combine dl = case dl [] of
            []  -> showString "mempty"
            [x] -> showString x
            xs  -> showParen (d > 6) $  -- infixr 6 <>
                   showDelim " <> " xs

        showDelim _     []     = id
        showDelim _     [x]    = showString x
        showDelim delim (x:xs) =
            showString x . showString delim . showDelim delim xs

-- | Test if an event is set in the event mask.  For example,
-- @'testEvent' 'evtWrite' ev@ tests if the 'evtWrite' event is
-- present in @ev@.
--
-- More precisely, test if at least one event is present in both masks.
testEvent :: Event -> Event -> Bool
testEvent (Event a) (Event b) = a .&. b /= 0

evtRead, evtWrite, evtExcept :: Event
evtRead   = Event #const EVT_READ
evtWrite  = Event #const EVT_WRITE
evtExcept = Event #const EVT_EXCEPT
