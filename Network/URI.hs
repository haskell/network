-----------------------------------------------------------------------------
-- |
-- Module      :  Network.URI
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (needs Text.Regex)
--
-- Implementation of RFC 2396  
--	"Uniform Resource Identifiers (URI): Generic Syntax"
--
-----------------------------------------------------------------------------

module Network.URI (
  URI(
      scheme,				-- :: String,
      authority,			-- :: String,
      path,				-- :: String,
      query,				-- :: String,
      fragment				-- :: String
     ), 
  -- instance Show URI

  parseURI,				-- :: String -> Maybe URI
	
  relativeTo,				-- :: URI -> URI -> Maybe URI

  -- support for putting strings into URI-friendly
  -- escaped format and getting them back again.
  -- Can't be done transparently, because certain characters
  -- have different meanings in different kinds of URI.
  reserved, unreserved, isAllowedInURI,	-- :: Char -> Bool
  escapeString,				-- :: String -> (Char->Bool) -> String
  unEscapeString			-- :: String -> String

  ) where

import Numeric
import Data.Char
import Text.Regex

-----------------------------------------------------------------------------
-- The URI datatype

data URI = URI
	{ 
	    scheme	:: String,
	    authority	:: String,
	    path	:: String,
	    query	:: String,
	    fragment	:: String
	}

instance Show URI where
  showsPrec _ uri = uriToString uri

-----------------------------------------------------------------------------
-- parseURI turns a string into a URI.  It returns Nothing if the
-- string isn't a valid URI.

parseURI :: String -> Maybe URI
parseURI s =
   let s1 = stripWS s in
   case matchRegex uriRegex s1 of
	Nothing -> Nothing
	Just (_:scheme:_:authority:path:_:query:_:fragment:_)
	   -> Just URI{
		  scheme    = scheme,
		  authority = authority,
		  path	    = path,
		  query	    = query,
		  fragment  = fragment
	         }
	_other ->
	   error "Network.URI.parseURI: internal error"		

-----------------------------------------------------------------------------
-- turning a URI back into a string

-- algorithm from part 7, sec 5.2, RFC 2396

uriToString :: URI -> ShowS
uriToString 
    URI{
	scheme=scheme,
	authority=authority,
	path=path,
	query=query,
	fragment=fragment
       } r
  = append ":" scheme (
    prepend "//" authority (
    append "" path (
    prepend "?" query (
    prepend "#" fragment r
    ))))

  where prepend pre "" rest = rest
	prepend pre s  rest = pre ++ s ++ rest
	
	append  post "" rest = rest
	append  post s  rest = s ++ post ++ rest

-- Regex from RFC 2396
uriRegex = mkRegex "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"

-----------------------------------------------------------------------------
-- character classes

reserved :: Char -> Bool
reserved c = c `elem` ";/?:@&=+$,"

-- can't use isAlphaNum etc. because these deal with ISO 8859 (and
-- possibly Unicode!) chars.
unreserved :: Char -> Bool
unreserved c = (c >= 'A' && c <= 'Z') 
	    || (c >= 'a' && c <= 'z')
	    || (c >= '0' && c <= '9')
	    || (c `elem` "-_.!~*'()")

isAllowedInURI :: Char -> Bool
isAllowedInURI c = reserved c || unreserved c || c == '%' -- escape char

escapeChar :: Char -> (Char->Bool) -> String
escapeChar c p | p c = [c]
	       | otherwise    = '%' : myShowHex (ord c) ""

escapeString :: String -> (Char->Bool) -> String
escapeString s p = foldr (\c cs -> escapeChar c p ++ cs) "" s

myShowHex :: Int -> ShowS
myShowHex n r
 =  case str of
	[]  -> "00"
	[c] -> ['0',c]
	cs  -> cs
 where
  str = showIntAtBase 16 (toChrHex) n r
  toChrHex d
    | d < 10    = chr (ord '0'   + fromIntegral d)
    | otherwise = chr (ord 'A' + fromIntegral (d - 10))

unEscapeString :: String -> String
unEscapeString [] = ""
unEscapeString ('%':x1:x2:s) | isHexDigit x1 && isHexDigit x2 
  = chr (hexDigit x1 * 16 + hexDigit x2) : unEscapeString s
unEscapeString (c:s) = c : unEscapeString s

hexDigit c | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
	   | c >= 'a' && c <= 'F' = ord c - ord 'a' + 10
	   | otherwise = ord c - ord '0'

-----------------------------------------------------------------------------
-- Resolving a relative URI relative to a base URI

-- algorithm from sec 5.2, RFC 2396

relativeTo :: URI -> URI -> Maybe URI
ref `relativeTo` base =
  -- ref has a scheme name, use it in its entirety.  Otherwise inherit
  -- the scheme name from base.
  if ref_scheme    /= ""  then Just ref else

  -- ref has an authority - we're done.  Otherwise inherit the authority.
  if ref_authority /= ""  then Just ref{scheme = base_scheme} else

  -- ref has an absolute path, we're done.
  if not (null ref_path) && head ref_path == '/'
	then Just ref{scheme = base_scheme, 
	              authority = base_authority} else
  
  -- relative path...
  let new_path = munge (dropLastComponent base_path ++ ref_path) []
  in if isErrorPath new_path 
	then Nothing 
	else Just ref{scheme = base_scheme, 
	              authority = base_authority,
	              path = new_path}
  where
       	URI{
	  scheme    = ref_scheme,
	  authority = ref_authority,
	  path      = ref_path,
	  query     = _ref_query,
	  fragment  = _ref_fragment
         } = ref

       	URI{
	  scheme    = base_scheme,
	  authority = base_authority,
	  path      = base_path,
	  query     = _base_query,
	  fragment  = _base_fragment
         } = base

	munge [] [] = ""
	munge [] ps = concat (reverse ps)
	munge ('.':'/':s)     ps     = munge s ps
	munge ['.']           ps     = munge [] ps
	munge ('.':'.':'/':s) (p:ps) | p /= "/" = munge s ps
	munge ['.','.']       (p:ps) = munge [] ps
	munge s		      ps     = munge rest' (p':ps)
		where (p,rest) = break (=='/') s
		      (p',rest') = case rest of
					'/':r -> (p++"/",r)
					r     -> (p,r)

	dropLastComponent = reverse . dropWhile (/= '/') . reverse

	isErrorPath ('/':'.':'.':'/':_) = True
	isErrorPath _ = False

stripLeadingWS, stripTrailingWS, stripWS :: String -> String
stripLeadingWS  = dropWhile isSpace
stripTrailingWS = reverse . stripLeadingWS . reverse
stripWS         = stripLeadingWS . stripTrailingWS

