-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Exception)
--
-- Haskell binding for CGI
--
-- Original Version by Erik Meijer <mailto:erik@cs.ruu.nl>
-- Further hacked on by Sven Panne <mailto:sven_panne@yahoo.com>
-- Further hacking by Andy Gill <mailto:andy@galconn.com>
--
-----------------------------------------------------------------------------

-- note: if using Windows, you might need to wrap 'withSocketsDo' round main.

module Network.CGI (
    Html, 
    wrapper,            -- ::           ([(String,String)] -> IO Html) -> IO ()
    pwrapper,           -- :: PortID -> ([(String,String)] -> IO Html) -> IO ()
    connectToCGIScript  -- :: String -> PortID -> IO ()
  ) where

import Data.Char ( ord, chr, toUpper, isDigit, isAlphaNum, isHexDigit )
import System.Environment ( getEnv )
import Control.Monad(MonadPlus(..), guard)
import System.IO

import Text.Html
import Control.Exception as Exception
import Control.Concurrent

import Network
import Network.Socket as Socket

-- ---------------------------------------------------------------------------
-- Yet another combinator parser library

-- NOTE: This is all a little bit of a sledgehammer here for the simple task
-- at hand...

-- The parser monad

newtype Parser a = Parser (String -> [(a,String)])

instance Functor Parser where
   -- map :: (a -> b) -> (Parser a -> Parser b)
   fmap f (Parser p) = Parser (\inp -> [(f v, out) | (v, out) <- p inp])

instance Monad Parser where
   -- return :: a -> Parser a
   return v = Parser (\inp -> [(v,inp)])

   -- >>= :: Parser a -> (a -> Parser b) -> Parser b
   (Parser p) >>= f = Parser (\inp -> concat [papply (f v) out
                                             | (v,out) <- p inp])

instance MonadPlus Parser where
   -- zero :: Parser a
   mzero = Parser (\_ -> [])
   -- (++) :: Parser a -> Parser a -> Parser a
   (Parser p) `mplus` (Parser q) = Parser (\inp -> (p inp ++ q inp))


-- Other primitive parser combinators

item :: Parser Char
item = Parser (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

force :: Parser a -> Parser a
force (Parser p) = Parser (\inp -> let x = p inp in
                             (fst (head x), snd (head x)) : tail x)

first :: Parser a -> Parser a
first (Parser p) = Parser (\inp -> case p inp of
                            []    -> []
                            (x:_) -> [x])

papply :: Parser a -> String -> [(a,String)]
papply (Parser p) inp = p inp


-- Derived combinators

plusplusplus :: Parser a -> Parser a -> Parser a
p `plusplusplus` q = first (p `mplus` q)

sat :: (Char -> Bool) -> Parser Char
sat p = do {x <- item; guard (p x); return x}

many :: Parser a -> Parser [a]
many p = force (many1 p `plusplusplus` return [])

many1 :: Parser a -> Parser [a]
many1 p = do {x <- p; xs <- many p; return (x:xs)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) `plusplusplus` return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x  <- p
                    xs <- many (do {sep; p})
                    return(x:xs)

char :: Char -> Parser Char
char x = sat (x==)

alphanum :: Parser Char
alphanum = sat isAlphaNum

string :: String -> Parser String
string []     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)

hexdigit :: Parser Char
hexdigit = sat isHexDigit

-- ---------------------------------------------------------------------------
-- Decoding application/x-www-form-urlencoded data

-- An URL encoded value consist of a sequence of
-- zero or more name "=" value pairs separated by "&"

-- Env ::= [Name "=" Value {"&" Name "=" Value}]

-- Names and values are URL-encoded,
-- according to the following table
-- 
--    character | encoding
--    ----------|---------
--     ' '      | '+'
--     '<'      | "%XX"
-- 	c       | "%"hexval(ord c)

urlDecode :: String -> [(String,String)]
urlDecode s = case papply env s of
                 [] -> []
                 ((e,_):_) -> e

env :: Parser [(String,String)]
env = (do n <- urlEncoded
          string "="
          v <- urlEncoded
          return (n,v)) `sepby` (string "&")

urlEncoded :: Parser String
urlEncoded
 = many ( alphanum `mplus` extra `mplus` safe
         `mplus` do{ char '+' ; return ' '}
         `mplus` do{ char '%'
		   ; d <- hexadecimal
		   ; return $ chr (hex2int d)
		   }
         )

extra :: Parser Char
extra = sat (`elem` "!*'(),")

safe :: Parser Char
safe = sat (`elem` "$-_.")

hexadecimal :: Parser HexString
hexadecimal = do d1 <- hexdigit
                 d2 <- hexdigit
                 return [d1,d2]

type HexString = String

hex2int :: HexString -> Int
hex2int ds = foldl (\n d -> n*16+d) 0 (map (toInt . toUpper) ds)
   where toInt d | isDigit d    =  ord d - ord '0'
         toInt d | isHexDigit d = (ord d - ord 'A') + 10
         toInt d                = error ("hex2int: illegal hex digit " ++ [d])

-- A function to do URL encoding and proving its correctness might be a
-- nice exercise for the book.
-- 
-- We don't usually need it for CGI scripts though. The browser does the
-- encoding and the CGI script does the decoding.

-- ---------------------------------------------------------------------------
-- Hide the CGI protocol from the programmer

wrapper :: ([(String,String)] -> IO Html) -> IO ()
wrapper f = do qs      <- getQueryString
               cgiVars <- getCgiVars
               a       <- f (cgiVars ++ urlDecode qs)
	       putStr "Content-type: text/html\n\n"
               putStr (renderHtml a)

getCgiVars :: IO [(String,String)]
getCgiVars = do vals <- mapM myGetEnv cgiVarNames
                return (zip cgiVarNames vals)

cgiVarNames :: [String]
cgiVarNames =
   [ "DOCUMENT_ROOT"
   , "AUTH_TYPE"
   , "GATEWAY_INTERFACE"
   , "SERVER_SOFTWARE"
   , "SERVER_NAME"
   , "REQUEST_METHOD"
   , "SERVER_ADMIN"
   , "SERVER_PORT"
   , "QUERY_STRING"
   , "CONTENT_LENGTH"
   , "CONTENT_TYPE"
   , "REMOTE_USER"
   , "REMOTE_IDENT"
   , "REMOTE_ADDR"
   , "REMOTE_HOST"
   , "TZ"
   , "PATH"
   , "PATH_INFO"
   , "PATH_TRANSLATED"
   , "SCRIPT_NAME"
   , "SCRIPT_FILENAME"
   , "HTTP_CONNECTION"
   , "HTTP_ACCEPT_LANGUAGE"
   , "HTTP_ACCEPT"
   , "HTTP_HOST"
   , "HTTP_UA_COLOR"
   , "HTTP_UA_CPU"
   , "HTTP_UA_OS"
   , "HTTP_UA_PIXELS"
   , "HTTP_USER_AGENT"
   ]                      


pwrapper :: PortID -> ([(String,String)] -> IO Html) -> IO ()
pwrapper pid f = 
       do { sock <- listenOn pid
          ; acceptConnections fn sock
          }
  where
       fn h = do { qs <- hGetLine h
                 ; a <- f (urlDecode qs)
	         ; hPutStr h "Content-type: text/html\n\n"
                 ; hPutStr h (renderHtml a)
                 }

acceptConnections fn sock = do
  (h, SockAddrInet port haddr) <- accept' sock
  forkIO (fn h `finally` (hClose h))
  acceptConnections fn sock

accept' :: Socket 		-- Listening Socket
       -> IO (Handle,SockAddr)	-- StdIO Handle for read/write
accept' sock = do
 (sock', addr) <- Socket.accept sock
 handle	<- socketToHandle sock' ReadWriteMode
 return (handle,addr)

-- ---------------------------------------------------------------------------
-- Small boot function for creating dummy cgi scripts

-- Sample program:
-- 
-- 	   main = connectToCGIScript "localhost" (PortNumber 3432)
-- 

connectToCGIScript :: String -> PortID -> IO ()
connectToCGIScript host portId
     = do { str <- getQueryString
          ; h <- connectTo host portId
                 `Exception.catch`
                   (\ e -> abort "Can not connect to CGI damon." e)
	  ; hPutStrLn h str
	  ; (sendBack h `finally` (hClose h))
               `Prelude.catch` (\e -> if isEOFError e
			              then return ()
                                      else ioError e)
          }

abort :: String -> Exception -> IO a
abort msg e = 
    do { putStrLn ("Content-type: text/html\n\n" ++
		   "<html><body>" ++ msg ++ "</body></html>")
       ; throw e
       }

sendBack h = do { s <- hGetLine h
                ; putStrLn s
		; sendBack h
                }

getQueryString :: IO String
getQueryString = do
   method <- myGetEnv "REQUEST_METHOD"
   case method of
      "POST" -> do len <- myGetEnv "CONTENT_LENGTH"
                   inp <- getContents
                   return (take (read len) inp)
      _      -> myGetEnv "QUERY_STRING"

myGetEnv :: String -> IO String
myGetEnv v = Prelude.catch (getEnv v) (const (return ""))
