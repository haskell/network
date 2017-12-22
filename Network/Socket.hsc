{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "HsNet.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).
--
-- Here are two minimal example programs using the TCP/IP protocol: a
-- server that echoes all data that it receives back (servicing only
-- one client) and a client using it.
--
-- > -- Echo server program
-- > module Main (main) where
-- >
-- > import Control.Concurrent (forkFinally)
-- > import qualified Control.Exception as E
-- > import Control.Monad (unless, forever, void)
-- > import qualified Data.ByteString as S
-- > import Network.Socket hiding (recv)
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >     addr <- resolve "3000"
-- >     E.bracket (open addr) close loop
-- >   where
-- >     resolve port = do
-- >         let hints = defaultHints {
-- >                 addrFlags = [AI_PASSIVE]
-- >               , addrSocketType = Stream
-- >               }
-- >         addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
-- >         return addr
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         setSocketOption sock ReuseAddr 1
-- >         bind sock (addrAddress addr)
-- >         listen sock 10
-- >         return sock
-- >     loop sock = forever $ do
-- >         (conn, peer) <- accept sock
-- >         putStrLn $ "Connection from " ++ show peer
-- >         void $ forkFinally (talk conn) (\_ -> close conn)
-- >     talk conn = do
-- >         msg <- recv conn 1024
-- >         unless (S.null msg) $ do
-- >           sendAll conn msg
-- >           talk conn
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > -- Echo client program
-- > module Main (main) where
-- >
-- > import qualified Control.Exception as E
-- > import qualified Data.ByteString.Char8 as C
-- > import Network.Socket hiding (recv)
-- > import Network.Socket.ByteString (recv, sendAll)
-- >
-- > main :: IO ()
-- > main = withSocketsDo $ do
-- >     addr <- resolve "127.0.0.1" "3000"
-- >     E.bracket (open addr) close talk
-- >   where
-- >     resolve host port = do
-- >         let hints = defaultHints { addrSocketType = Stream }
-- >         addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
-- >         return addr
-- >     open addr = do
-- >         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- >         connect sock $ addrAddress addr
-- >         return sock
-- >     talk sock = do
-- >         sendAll sock "Hello, world!"
-- >         msg <- recv sock 1024
-- >         putStr "Received: "
-- >         C.putStrLn msg
-----------------------------------------------------------------------------

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket
    (
    -- * Types
    -- ** Socket
      Socket
    , socketFd
    , socketFamily
    , socketType
    , socketProtocol
    , socketStatus
    , socketPort
    , mkSocket
    -- ** Socket status
    , SocketStatus(..)
    -- ** Family
    , Family(..)
    , isSupportedFamily
    -- ** Socket type
    , SocketType(..)
    , isSupportedSocketType
    -- ** Socket address
    , SockAddr(..)
    , isSupportedSockAddr
    -- ** Host address
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
#if defined(IPV6_SOCKET_SUPPORT)
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID
#endif
    -- ** Protocol number
    , ProtocolNumber
    , defaultProtocol
    -- ** Port number
    , PortNumber

    -- * Address operations

    , HostName
    , ServiceName

#if defined(IPV6_SOCKET_SUPPORT)
    -- ** getaddrinfo
    , AddrInfo(..)

    , AddrInfoFlag(..)
    , addrInfoFlagImplemented

    , defaultHints

    , getAddrInfo

    -- ** getnameinfo
    , NameInfoFlag(..)

    , getNameInfo
#endif

    -- * Socket operations
    , socket
#if defined(DOMAIN_SOCKET_SUPPORT)
    , socketPair
#endif
    , connect
    , bind
    , listen
    , accept
    , getPeerName
    , getSocketName

#if defined(HAVE_STRUCT_UCRED) || defined(HAVE_GETPEEREID)
    -- get the credentials of our domain socket peer.
    , getPeerCred
#if defined(HAVE_GETPEEREID)
    , getPeerEid
#endif
#endif

    -- ** Sending and receiving data
    , sendBuf
    , recvBuf
    , sendBufTo
    , recvBufFrom

    -- ** Closing
    , close
    , shutdown
    , ShutdownCmd(..)

    -- ** Predicates on sockets
    , isConnected
    , isBound
    , isListening
    , isReadable
    , isWritable

    -- * Socket options
    , SocketOption(..)
    , isSupportedSocketOption
    , getSocketOption
    , setSocketOption

    -- * File descriptor transmission
#ifdef DOMAIN_SOCKET_SUPPORT
    , sendFd
    , recvFd

#endif

    -- * Special constants
    , aNY_PORT
    , iNADDR_ANY
#if defined(IPV6_SOCKET_SUPPORT)
    , iN6ADDR_ANY
#endif
    , sOMAXCONN
    , sOL_SOCKET
#ifdef SCM_RIGHTS
    , sCM_RIGHTS
#endif
    , maxListenQueue

    -- * Initialisation
    , withSocketsDo

    -- * Low level operations
    -- in case you ever want to get at the underlying file descriptor..
    , setNonBlockIfNeeded

    -- * Deprecated
    , send
    , sendTo
    , recv
    , recvLen
    , recvFrom
    , inet_addr
    , inet_ntoa
    , htonl
    , ntohl
    , socketToHandle
    ) where

import Network.Socket.Buffer
import Network.Socket.Close
import Network.Socket.Constant
import Network.Socket.Handle
import Network.Socket.Info
import Network.Socket.Internal
import Network.Socket.Name
import Network.Socket.Options
import Network.Socket.String
import Network.Socket.Syscall
import Network.Socket.Types
