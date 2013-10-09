{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
--  Module      :  Network.URI
--  Copyright   :  (c) 2004, Graham Klyne
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Graham Klyne <gk@ninebynine.org>
--  Stability   :  provisional
--  Portability :  portable
--
--  This module defines functions for handling URIs.  It presents substantially the
--  same interface as the older GHC Network.URI module, but is implemented using
--  Parsec rather than a Regex library that is not available with Hugs.  The internal
--  representation of URI has been changed so that URI strings are more
--  completely preserved when round-tripping to a URI value and back.
--
--  In addition, four methods are provided for parsing different
--  kinds of URI string (as noted in RFC3986):
--      'parseURI',
--      'parseURIReference',
--      'parseRelativeReference' and
--      'parseAbsoluteURI'.
--
--  Further, four methods are provided for classifying different
--  kinds of URI string (as noted in RFC3986):
--      'isURI',
--      'isURIReference',
--      'isRelativeReference' and
--      'isAbsoluteURI'.
--
--  The long-standing official reference for URI handling was RFC2396 [1],
--  as updated by RFC 2732 [2], but this was replaced by a new specification,
--  RFC3986 [3] in January 2005.  This latter specification has been used
--  as the primary reference for constructing the URI parser implemented
--  here, and it is intended that there is a direct relationship between
--  the syntax definition in that document and this parser implementation.
--
--  RFC 1808 [4] contains a number of test cases for relative URI handling.
--  Dan Connolly's Python module @uripath.py@ [5] also contains useful details
--  and test cases.
--
--  Some of the code has been copied from the previous GHC implementation,
--  but the parser is replaced with one that performs more complete
--  syntax checking of the URI itself, according to RFC3986 [3].
--
--  References
--
--  (1) <http://www.ietf.org/rfc/rfc2396.txt>
--
--  (2) <http://www.ietf.org/rfc/rfc2732.txt>
--
--  (3) <http://www.ietf.org/rfc/rfc3986.txt>
--
--  (4) <http://www.ietf.org/rfc/rfc1808.txt>
--
--  (5) <http://www.w3.org/2000/10/swap/uripath.py>
--
--------------------------------------------------------------------------------

module Network.URI
    (
    -- * The URI type
      URI(..)
    , URIAuth(..)
    , nullURI
      
    -- * Parsing
    , parseURI
    , parseURIReference
    , parseRelativeReference
    , parseAbsoluteURI
      
    -- * Test for strings containing various kinds of URI
    , isURI
    , isURIReference
    , isRelativeReference
    , isAbsoluteURI
    , isIPv6address
    , isIPv4address
      
    -- * Predicates
    , uriIsAbsolute
    , uriIsRelative
      
    -- * Relative URIs
    , relativeTo
    , nonStrictRelativeTo
    , relativeFrom
      
    -- * Operations on URI strings
    -- | Support for putting strings into URI-friendly
    --   escaped format and getting them back again.
    --   This can't be done transparently in all cases, because certain
    --   characters have different meanings in different kinds of URI.
    --   The URI spec [3], section 2.4, indicates that all URI components
    --   should be escaped before they are assembled as a URI:
    --   \"Once produced, a URI is always in its percent-encoded form\"
    , uriToString
    , isReserved, isUnreserved
    , isAllowedInURI, isUnescapedInURI
    , isUnescapedInURIComponent
    , escapeURIChar
    , escapeURIString
    , unEscapeString
      
    -- * URI Normalization functions
    , normalizeCase
    , normalizeEscape
    , normalizePathSegments
      
    -- * Deprecated functions
    , parseabsoluteURI
    , escapeString
    , reserved, unreserved
    , scheme, authority, path, query, fragment
    ) where

import Text.ParserCombinators.Parsec
    ( GenParser, ParseError
    , parse, (<|>), (<?>), try
    , option, many, many1, count, notFollowedBy
    , char, satisfy, oneOf, string, eof
    , unexpected
    )

import Control.Monad (MonadPlus(..))
import Data.Char (ord, chr, isHexDigit, toLower, toUpper, digitToInt)
import Data.Bits ((.|.),(.&.),shiftL,shiftR)
import Debug.Trace (trace)
import Numeric (showIntAtBase)

import Data.Typeable (Typeable)
#if MIN_VERSION_base(4,0,0)
import Data.Data (Data)
#else
import Data.Generics (Data)
#endif

------------------------------------------------------------
--  The URI datatype
------------------------------------------------------------

-- |Represents a general universal resource identifier using
--  its component parts.
--
--  For example, for the URI
--
--  >   foo://anonymous@www.haskell.org:42/ghc?query#frag
--
--  the components are:
--
data URI = URI
    { uriScheme     :: String           -- ^ @foo:@
    , uriAuthority  :: Maybe URIAuth    -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: String           -- ^ @\/ghc@
    , uriQuery      :: String           -- ^ @?query@
    , uriFragment   :: String           -- ^ @#frag@
    } deriving (Eq, Ord, Typeable, Data)

-- |Type for authority value within a URI
data URIAuth = URIAuth
    { uriUserInfo   :: String           -- ^ @anonymous\@@
    , uriRegName    :: String           -- ^ @www.haskell.org@
    , uriPort       :: String           -- ^ @:42@
    } deriving (Eq, Ord, Show, Typeable, Data)

-- |Blank URI
nullURI :: URI
nullURI = URI
    { uriScheme     = ""
    , uriAuthority  = Nothing
    , uriPath       = ""
    , uriQuery      = ""
    , uriFragment   = ""
    }

--  URI as instance of Show.  Note that for security reasons, the default
--  behaviour is to suppress any userinfo field (see RFC3986, section 7.5).
--  This can be overridden by using uriToString directly with first
--  argument @id@ (noting that this returns a ShowS value rather than a string).
--
--  [[[Another design would be to embed the userinfo mapping function in
--  the URIAuth value, with the default value suppressing userinfo formatting,
--  but providing a function to return a new URI value with userinfo
--  data exposed by show.]]]
--
instance Show URI where
    showsPrec _ = uriToString defaultUserInfoMap

defaultUserInfoMap :: String -> String
defaultUserInfoMap uinf = user++newpass
    where
        (user,pass) = break (==':') uinf
        newpass     = if null pass || (pass == "@")
                                   || (pass == ":@")
                        then pass
                        else ":...@"

testDefaultUserInfoMap :: [Bool]
testDefaultUserInfoMap =
     [ defaultUserInfoMap ""                == ""
     , defaultUserInfoMap "@"               == "@"
     , defaultUserInfoMap "user@"           == "user@"
     , defaultUserInfoMap "user:@"          == "user:@"
     , defaultUserInfoMap "user:anonymous@" == "user:...@"
     , defaultUserInfoMap "user:pass@"      == "user:...@"
     , defaultUserInfoMap "user:pass"       == "user:...@"
     , defaultUserInfoMap "user:anonymous"  == "user:...@"
     ]

------------------------------------------------------------
--  Parse a URI
------------------------------------------------------------

-- |Turn a string containing a URI into a 'URI'.
--  Returns 'Nothing' if the string is not a valid URI;
--  (an absolute URI with optional fragment identifier).
--
--  NOTE: this is different from the previous network.URI,
--  whose @parseURI@ function works like 'parseURIReference'
--  in this module.
--
parseURI :: String -> Maybe URI
parseURI = parseURIAny uri

-- |Parse a URI reference to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid URI reference.
--  (an absolute or relative URI with optional fragment identifier).
--
parseURIReference :: String -> Maybe URI
parseURIReference = parseURIAny uriReference

-- |Parse a relative URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid relative URI.
--  (a relative URI with optional fragment identifier).
--
parseRelativeReference :: String -> Maybe URI
parseRelativeReference = parseURIAny relativeRef

-- |Parse an absolute URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid absolute URI.
--  (an absolute URI without a fragment identifier).
--
parseAbsoluteURI :: String -> Maybe URI
parseAbsoluteURI = parseURIAny absoluteURI

-- |Test if string contains a valid URI
--  (an absolute URI with optional fragment identifier).
--
isURI :: String -> Bool
isURI = isValidParse uri

-- |Test if string contains a valid URI reference
--  (an absolute or relative URI with optional fragment identifier).
--
isURIReference :: String -> Bool
isURIReference = isValidParse uriReference

-- |Test if string contains a valid relative URI
--  (a relative URI with optional fragment identifier).
--
isRelativeReference :: String -> Bool
isRelativeReference = isValidParse relativeRef

-- |Test if string contains a valid absolute URI
--  (an absolute URI without a fragment identifier).
--
isAbsoluteURI :: String -> Bool
isAbsoluteURI = isValidParse absoluteURI

-- |Test if string contains a valid IPv6 address
--
isIPv6address :: String -> Bool
isIPv6address = isValidParse ipv6address

-- |Test if string contains a valid IPv4 address
--
isIPv4address :: String -> Bool
isIPv4address = isValidParse ipv4address

-- |Test function: parse and reconstruct a URI reference
--
testURIReference :: String -> String
testURIReference uristr = show (parseAll uriReference "" uristr)

--  Helper function for turning a string into a URI
--
parseURIAny :: URIParser URI -> String -> Maybe URI
parseURIAny parser uristr = case parseAll parser "" uristr of
        Left  _ -> Nothing
        Right u -> Just u

--  Helper function to test a string match to a parser
--
isValidParse :: URIParser a -> String -> Bool
isValidParse parser uristr = case parseAll parser "" uristr of
        -- Left  e -> error (show e)
        Left  _ -> False
        Right _ -> True

parseAll :: URIParser a -> String -> String -> Either ParseError a
parseAll parser filename uristr = parse newparser filename uristr
    where
        newparser =
            do  { res <- parser
                ; eof
                ; return res
                }

------------------------------------------------------------
--  Predicates
------------------------------------------------------------

uriIsAbsolute :: URI -> Bool
uriIsAbsolute (URI {uriScheme = scheme}) = scheme /= ""

uriIsRelative :: URI -> Bool
uriIsRelative = not . uriIsAbsolute

------------------------------------------------------------
--  URI parser body based on Parsec elements and combinators
------------------------------------------------------------

--  Parser parser type.
--  Currently
type URIParser a = GenParser Char () a

--  RFC3986, section 2.1
--
--  Parse and return a 'pct-encoded' sequence
--
escaped :: URIParser String
escaped =
    do  { char '%'
        ; h1 <- hexDigitChar
        ; h2 <- hexDigitChar
        ; return $ ['%',h1,h2]
        }

--  RFC3986, section 2.2
--
-- |Returns 'True' if the character is a \"reserved\" character in a
--  URI.  To include a literal instance of one of these characters in a
--  component of a URI, it must be escaped.
--
isReserved :: Char -> Bool
isReserved c = isGenDelims c || isSubDelims c

isGenDelims :: Char -> Bool
isGenDelims c = c `elem` ":/?#[]@"

isSubDelims :: Char -> Bool
isSubDelims c = c `elem` "!$&'()*+,;="

genDelims :: URIParser String
genDelims = do { c <- satisfy isGenDelims ; return [c] }

subDelims :: URIParser String
subDelims = do { c <- satisfy isSubDelims ; return [c] }

--  RFC3986, section 2.3
--
-- |Returns 'True' if the character is an \"unreserved\" character in
--  a URI.  These characters do not need to be escaped in a URI.  The
--  only characters allowed in a URI are either \"reserved\",
--  \"unreserved\", or an escape sequence (@%@ followed by two hex digits).
--
isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` "-_.~")

unreservedChar :: URIParser String
unreservedChar = do { c <- satisfy isUnreserved ; return [c] }

--  RFC3986, section 3
--
--   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
--
--   hier-part   = "//" authority path-abempty
--               / path-abs
--               / path-rootless
--               / path-empty

uri :: URIParser URI
uri =
    do  { us <- try uscheme
        -- ; ua <- option Nothing ( do { try (string "//") ; uauthority } )
        -- ; up <- upath
        ; (ua,up) <- hierPart
        ; uq <- option "" ( do { char '?' ; uquery    } )
        ; uf <- option "" ( do { char '#' ; ufragment } )
        ; return $ URI
            { uriScheme    = us
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = uf
            }
        }

hierPart :: URIParser ((Maybe URIAuth),String)
hierPart =
        do  { try (string "//")
            ; ua <- uauthority
            ; up <- pathAbEmpty
            ; return (ua,up)
            }
    <|> do  { up <- pathAbs
            ; return (Nothing,up)
            }
    <|> do  { up <- pathRootLess
            ; return (Nothing,up)
            }
    <|> do  { return (Nothing,"")
            }

--  RFC3986, section 3.1

uscheme :: URIParser String
uscheme =
    do  { s <- oneThenMany alphaChar (satisfy isSchemeChar)
        ; char ':'
        ; return $ s++":"
        }

--  RFC3986, section 3.2

uauthority :: URIParser (Maybe URIAuth)
uauthority =
    do  { uu <- option "" (try userinfo)
        ; uh <- host
        ; up <- option "" port
        ; return $ Just $ URIAuth
            { uriUserInfo = uu
            , uriRegName  = uh
            , uriPort     = up
            }
        }

--  RFC3986, section 3.2.1

userinfo :: URIParser String
userinfo =
    do  { uu <- many (uchar ";:&=+$,")
        ; char '@'
        ; return (concat uu ++"@")
        }

--  RFC3986, section 3.2.2

host :: URIParser String
host = ipLiteral <|> try ipv4address <|> regName

ipLiteral :: URIParser String
ipLiteral =
    do  { char '['
        ; ua <- ( ipv6address <|> ipvFuture )
        ; char ']'
        ; return $ "[" ++ ua ++ "]"
        }
    <?> "IP address literal"

ipvFuture :: URIParser String
ipvFuture =
    do  { char 'v'
        ; h <- hexDigitChar
        ; char '.'
        ; a <- many1 (satisfy isIpvFutureChar)
        ; return $ 'v':h:'.':a
        }

isIpvFutureChar :: Char -> Bool
isIpvFutureChar c = isUnreserved c || isSubDelims c || (c==';')

ipv6address :: URIParser String
ipv6address =
        try ( do
                { a2 <- count 6 h4c
                ; a3 <- ls32
                ; return $ concat a2 ++ a3
                } )
    <|> try ( do
                { string "::"
                ; a2 <- count 5 h4c
                ; a3 <- ls32
                ; return $ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 0
                ; string "::"
                ; a2 <- count 4 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 1
                ; string "::"
                ; a2 <- count 3 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 2
                ; string "::"
                ; a2 <- count 2 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 3
                ; string "::"
                ; a2 <- h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 4
                ; string "::"
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 5
                ; string "::"
                ; a3 <- h4
                ; return $ a1 ++ "::" ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 6
                ; string "::"
                ; return $ a1 ++ "::"
                } )
    <?> "IPv6 address"

opt_n_h4c_h4 :: Int -> URIParser String
opt_n_h4c_h4 n = option "" $
    do  { a1 <- countMinMax 0 n h4c
        ; a2 <- h4
        ; return $ concat a1 ++ a2
        }

ls32 :: URIParser String
ls32 =  try ( do
                { a1 <- h4c
                ; a2 <- h4
                ; return (a1++a2)
                } )
    <|> ipv4address

h4c :: URIParser String
h4c = try $
    do  { a1 <- h4
        ; char ':'
        ; notFollowedBy (char ':')
        ; return $ a1 ++ ":"
        }

h4 :: URIParser String
h4 = countMinMax 1 4 hexDigitChar

ipv4address :: URIParser String
ipv4address =
    do  { a1 <- decOctet ; char '.'
        ; a2 <- decOctet ; char '.'
        ; a3 <- decOctet ; char '.'
        ; a4 <- decOctet
        ; notFollowedBy regName
        ; return $ a1++"."++a2++"."++a3++"."++a4
        }
    <?> "IPv4 Address"

decOctet :: URIParser String
decOctet =
    do  { a1 <- countMinMax 1 3 digitChar
        ; if (read a1 :: Integer) > 255 then
            fail "Decimal octet value too large"
          else
            return a1
        }

regName :: URIParser String
regName =
    do  { ss <- countMinMax 0 255 ( unreservedChar <|> escaped <|> subDelims )
        ; return $ concat ss
        }
    <?> "Registered name"

--  RFC3986, section 3.2.3

port :: URIParser String
port =
    do  { char ':'
        ; p <- many digitChar
        ; return (':':p)
        }

--
--  RFC3986, section 3.3
--
--   path          = path-abempty    ; begins with "/" or is empty
--                 / path-abs        ; begins with "/" but not "//"
--                 / path-noscheme   ; begins with a non-colon segment
--                 / path-rootless   ; begins with a segment
--                 / path-empty      ; zero characters
--
--   path-abempty  = *( "/" segment )
--   path-abs      = "/" [ segment-nz *( "/" segment ) ]
--   path-noscheme = segment-nzc *( "/" segment )
--   path-rootless = segment-nz *( "/" segment )
--   path-empty    = 0<pchar>
--
--   segment       = *pchar
--   segment-nz    = 1*pchar
--   segment-nzc   = 1*( unreserved / pct-encoded / sub-delims / "@" )
--
--   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

{-
upath :: URIParser String
upath = pathAbEmpty
    <|> pathAbs
    <|> pathNoScheme
    <|> pathRootLess
    <|> pathEmpty
-}

pathAbEmpty :: URIParser String
pathAbEmpty =
    do  { ss <- many slashSegment
        ; return $ concat ss
        }

pathAbs :: URIParser String
pathAbs =
    do  { char '/'
        ; ss <- option "" pathRootLess
        ; return $ '/':ss
        }

pathNoScheme :: URIParser String
pathNoScheme =
    do  { s1 <- segmentNzc
        ; ss <- many slashSegment
        ; return $ concat (s1:ss)
        }

pathRootLess :: URIParser String
pathRootLess =
    do  { s1 <- segmentNz
        ; ss <- many slashSegment
        ; return $ concat (s1:ss)
        }

slashSegment :: URIParser String
slashSegment =
    do  { char '/'
        ; s <- segment
        ; return ('/':s)
        }

segment :: URIParser String
segment =
    do  { ps <- many pchar
        ; return $ concat ps
        }

segmentNz :: URIParser String
segmentNz =
    do  { ps <- many1 pchar
        ; return $ concat ps
        }

segmentNzc :: URIParser String
segmentNzc =
    do  { ps <- many1 (uchar "@")
        ; return $ concat ps
        }

pchar :: URIParser String
pchar = uchar ":@"

-- helper function for pchar and friends
uchar :: String -> URIParser String
uchar extras =
        unreservedChar
    <|> escaped
    <|> subDelims
    <|> do { c <- oneOf extras ; return [c] }

--  RFC3986, section 3.4

uquery :: URIParser String
uquery =
    do  { ss <- many $ uchar (":@"++"/?")
        ; return $ '?':concat ss
        }

--  RFC3986, section 3.5

ufragment :: URIParser String
ufragment =
    do  { ss <- many $ uchar (":@"++"/?")
        ; return $ '#':concat ss
        }

--  Reference, Relative and Absolute URI forms
--
--  RFC3986, section 4.1

uriReference :: URIParser URI
uriReference = uri <|> relativeRef

--  RFC3986, section 4.2
--
--   relative-URI  = relative-part [ "?" query ] [ "#" fragment ]
--
--   relative-part = "//" authority path-abempty
--                 / path-abs
--                 / path-noscheme
--                 / path-empty

relativeRef :: URIParser URI
relativeRef =
    do  { notMatching uscheme
        -- ; ua <- option Nothing ( do { try (string "//") ; uauthority } )
        -- ; up <- upath
        ; (ua,up) <- relativePart
        ; uq <- option "" ( do { char '?' ; uquery    } )
        ; uf <- option "" ( do { char '#' ; ufragment } )
        ; return $ URI
            { uriScheme    = ""
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = uf
            }
        }

relativePart :: URIParser ((Maybe URIAuth),String)
relativePart =
        do  { try (string "//")
            ; ua <- uauthority
            ; up <- pathAbEmpty
            ; return (ua,up)
            }
    <|> do  { up <- pathAbs
            ; return (Nothing,up)
            }
    <|> do  { up <- pathNoScheme
            ; return (Nothing,up)
            }
    <|> do  { return (Nothing,"")
            }

--  RFC3986, section 4.3

absoluteURI :: URIParser URI
absoluteURI =
    do  { us <- uscheme
        -- ; ua <- option Nothing ( do { try (string "//") ; uauthority } )
        -- ; up <- upath
        ; (ua,up) <- hierPart
        ; uq <- option "" ( do { char '?' ; uquery    } )
        ; return $ URI
            { uriScheme    = us
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = ""
            }
        }

--  Imports from RFC 2234

    -- NOTE: can't use isAlphaNum etc. because these deal with ISO 8859
    -- (and possibly Unicode!) chars.
    -- [[[Above was a comment originally in GHC Network/URI.hs:
    --    when IRIs are introduced then most codepoints above 128(?) should
    --    be treated as unreserved, and higher codepoints for letters should
    --    certainly be allowed.
    -- ]]]

isAlphaChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isDigitChar :: Char -> Bool
isDigitChar c    = (c >= '0' && c <= '9')

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isHexDigitChar :: Char -> Bool
isHexDigitChar c = isHexDigit c

isSchemeChar :: Char -> Bool
isSchemeChar c   = (isAlphaNumChar c) || (c `elem` "+-.")

alphaChar :: URIParser Char
alphaChar = satisfy isAlphaChar         -- or: Parsec.letter ?

digitChar :: URIParser Char
digitChar = satisfy isDigitChar         -- or: Parsec.digit ?

alphaNumChar :: URIParser Char
alphaNumChar = satisfy isAlphaNumChar

hexDigitChar :: URIParser Char
hexDigitChar = satisfy isHexDigitChar   -- or: Parsec.hexDigit ?

--  Additional parser combinators for common patterns

oneThenMany :: GenParser t s a -> GenParser t s a -> GenParser t s [a]
oneThenMany p1 pr =
    do  { a1 <- p1
        ; ar <- many pr
        ; return (a1:ar)
        }

countMinMax :: Int -> Int -> GenParser t s a -> GenParser t s [a]
countMinMax m n p | m > 0 =
    do  { a1 <- p
        ; ar <- countMinMax (m-1) (n-1) p
        ; return (a1:ar)
        }
countMinMax _ n _ | n <= 0 = return []
countMinMax _ n p = option [] $
    do  { a1 <- p
        ; ar <- countMinMax 0 (n-1) p
        ; return (a1:ar)
        }

notMatching :: Show a => GenParser tok st a -> GenParser tok st ()
notMatching p = do { a <- try p ; unexpected (show a) } <|> return ()

------------------------------------------------------------
--  Reconstruct a URI string
------------------------------------------------------------
--
-- |Turn a 'URI' into a string.
--
--  Uses a supplied function to map the userinfo part of the URI.
--
--  The Show instance for URI uses a mapping that hides any password
--  that may be present in the URI.  Use this function with argument @id@
--  to preserve the password in the formatted output.
--
uriToString :: (String->String) -> URI -> ShowS
uriToString userinfomap URI { uriScheme=myscheme
                            , uriAuthority=myauthority
                            , uriPath=mypath
                            , uriQuery=myquery
                            , uriFragment=myfragment
                            } =
    (myscheme++) . (uriAuthToString userinfomap myauthority)
               . (mypath++) . (myquery++) . (myfragment++)

uriAuthToString :: (String->String) -> (Maybe URIAuth) -> ShowS
uriAuthToString _           Nothing   = id          -- shows ""
uriAuthToString userinfomap
        (Just URIAuth { uriUserInfo = myuinfo
                      , uriRegName  = myregname
                      , uriPort     = myport
                      } ) =
    ("//"++) . (if null myuinfo then id else ((userinfomap myuinfo)++))
             . (myregname++)
             . (myport++)

------------------------------------------------------------
--  Character classes
------------------------------------------------------------

-- | Returns 'True' if the character is allowed in a URI.
--
isAllowedInURI :: Char -> Bool
isAllowedInURI c = isReserved c || isUnreserved c || c == '%' -- escape char

-- | Returns 'True' if the character is allowed unescaped in a URI.
--
isUnescapedInURI :: Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c

-- | Returns 'True' if the character is allowed unescaped in a URI component.
--
isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (isReserved c || not (isUnescapedInURI c))

------------------------------------------------------------
--  Escape sequence handling
------------------------------------------------------------

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeURIChar :: (Char->Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 (toChrHex) n r of
            []  -> "00"
            [x] -> ['0',x]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- From http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
-- Returns [Int] for use with showIntAtBase
utf8EncodeChar :: Char -> [Int]
utf8EncodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- |Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> String           -- ^ the string to process
    -> String           -- ^ the resulting URI string
escapeURIString p s = concatMap (escapeURIChar p) s

-- |Turns all instances of escaped characters in the string back
--  into literal characters.
--
unEscapeString :: String -> String
unEscapeString [] = ""
unEscapeString s@(c:cs) = case unEscapeByte s of
    Just (byte, rest) -> unEscapeUtf8 byte rest
    Nothing -> c : unEscapeString cs

unEscapeByte :: String -> Maybe (Int, String)
unEscapeByte ('%':x1:x2:s) | isHexDigit x1 && isHexDigit x2 =
    Just (digitToInt x1 * 16 + digitToInt x2, s)
unEscapeByte _ = Nothing

-- Adapted from http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
unEscapeUtf8 :: Int -> String -> String
unEscapeUtf8 c rest
    | c < 0x80 = chr c : unEscapeString rest
    | c < 0xc0 = replacement_character : unEscapeString rest
    | c < 0xe0 = multi1
    | c < 0xf0 = multi_byte 2 0xf 0x800
    | c < 0xf8 = multi_byte 3 0x7 0x10000
    | c < 0xfc = multi_byte 4 0x3 0x200000
    | c < 0xfe = multi_byte 5 0x1 0x4000000
    | otherwise    = replacement_character : unEscapeString rest
    where
    replacement_character = '\xfffd'
    multi1 = case unEscapeByte rest of
      Just (c1, ds) | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : unEscapeString ds
                            else replacement_character : unEscapeString ds
      _ -> replacement_character : unEscapeString rest

    multi_byte i mask overlong =
      aux i rest (unEscapeByte rest) (c .&. mask)
      where
        aux 0 rs _ acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : unEscapeString rs
          | otherwise = replacement_character : unEscapeString rs

        aux n _ (Just (r, rs)) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs (unEscapeByte rs)
                               $! shiftL acc 6 .|. (r .&. 0x3f)

        aux _ rs _ _ = replacement_character : unEscapeString rs

------------------------------------------------------------
-- Resolving a relative URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the value of the
--  first 'URI' interpreted as relative to the second 'URI'.
--  For example:
--
--  > "foo" `relativeTo` "http://bar.org/" = "http://bar.org/foo"
--  > "http:foo" `nonStrictRelativeTo` "http://bar.org/" = "http://bar.org/foo"
--
--  Algorithm from RFC3986 [3], section 5.2.2
--

nonStrictRelativeTo :: URI -> URI -> URI
nonStrictRelativeTo ref base = relativeTo ref' base
    where
        ref' = if uriScheme ref == uriScheme base
               then ref { uriScheme="" }
               else ref

isDefined :: ( MonadPlus m, Eq (m a) ) => m a -> Bool
isDefined a = a /= mzero

-- | Returns a new 'URI' which represents the value of the first 'URI'
-- interpreted as relative to the second 'URI'.
--
-- Algorithm from RFC3986 [3], section 5.2
relativeTo :: URI -> URI -> URI
relativeTo ref base
    | isDefined ( uriScheme ref ) =
        just_segments ref
    | isDefined ( uriAuthority ref ) =
        just_segments ref { uriScheme = uriScheme base }
    | isDefined ( uriPath ref ) =
        if (head (uriPath ref) == '/') then
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                }
        else
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                , uriPath      = mergePaths base ref
                }
    | isDefined ( uriQuery ref ) =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            }
    | otherwise =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            , uriQuery     = uriQuery base
            }
    where
        just_segments u =
            u { uriPath = removeDotSegments (uriPath u) }
        mergePaths b r
            | isDefined (uriAuthority b) && null pb = '/':pr
            | otherwise                             = dropLast pb ++ pr
            where
                pb = uriPath b
                pr = uriPath r
        dropLast = fst . splitLast -- reverse . dropWhile (/='/') . reverse

--  Remove dot segments, but protect leading '/' character
removeDotSegments :: String -> String
removeDotSegments ('/':ps) = '/':elimDots ps []
removeDotSegments ps       = elimDots ps []

--  Second arg accumulates segments processed so far in reverse order
elimDots :: String -> [String] -> String
-- elimDots ps rs | traceVal "\nps " ps $ traceVal "rs " rs $ False = error ""
elimDots [] [] = ""
elimDots [] rs = concat (reverse rs)
elimDots (    '.':'/':ps)     rs = elimDots ps rs
elimDots (    '.':[]    )     rs = elimDots [] rs
elimDots (    '.':'.':'/':ps) rs = elimDots ps (drop 1 rs)
elimDots (    '.':'.':[]    ) rs = elimDots [] (drop 1 rs)
elimDots ps rs = elimDots ps1 (r:rs)
    where
        (r,ps1) = nextSegment ps

--  Returns the next segment and the rest of the path from a path string.
--  Each segment ends with the next '/' or the end of string.
--
nextSegment :: String -> (String,String)
nextSegment ps =
    case break (=='/') ps of
        (r,'/':ps1) -> (r++"/",ps1)
        (r,_)       -> (r,[])

--  Split last (name) segment from path, returning (path,name)
splitLast :: String -> (String,String)
splitLast p = (reverse revpath,reverse revname)
    where
        (revname,revpath) = break (=='/') $ reverse p

------------------------------------------------------------
-- Finding a URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the relative location of
--  the first 'URI' with respect to the second 'URI'.  Thus, the
--  values supplied are expected to be absolute URIs, and the result
--  returned may be a relative URI.
--
--  Example:
--
--  > "http://example.com/Root/sub1/name2#frag"
--  >   `relativeFrom` "http://example.com/Root/sub2/name2#frag"
--  >   == "../sub1/name2#frag"
--
--  There is no single correct implementation of this function,
--  but any acceptable implementation must satisfy the following:
--
--  > (uabs `relativeFrom` ubase) `relativeTo` ubase == uabs
--
--  For any valid absolute URI.
--  (cf. <http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html>
--       <http://lists.w3.org/Archives/Public/uri/2003Jan/0005.html>)
--
relativeFrom :: URI -> URI -> URI
relativeFrom uabs base
    | diff uriScheme    uabs base = uabs
    | diff uriAuthority uabs base = uabs { uriScheme = "" }
    | diff uriPath      uabs base = uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = relPathFrom (removeBodyDotSegments $ uriPath uabs)
                                     (removeBodyDotSegments $ uriPath base)
        }
    | diff uriQuery     uabs base = uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = ""
        }
    | otherwise = uabs          -- Always carry fragment from uabs
        { uriScheme    = ""
        , uriAuthority = Nothing
        , uriPath      = ""
        , uriQuery     = ""
        }
    where
        diff :: Eq b => (a -> b) -> a -> a -> Bool
        diff sel u1 u2 = sel u1 /= sel u2
        -- Remove dot segments except the final segment
        removeBodyDotSegments p = removeDotSegments p1 ++ p2
            where
                (p1,p2) = splitLast p

relPathFrom :: String -> String -> String
relPathFrom []   _    = "/"
relPathFrom pabs []   = pabs
relPathFrom pabs base =                 -- Construct a relative path segments
    if sa1 == sb1                       -- if the paths share a leading segment
        then if (sa1 == "/")            -- other than a leading '/'
            then if (sa2 == sb2)
                then relPathFrom1 ra2 rb2
                else pabs
            else relPathFrom1 ra1 rb1
        else pabs
    where
        (sa1,ra1) = nextSegment pabs
        (sb1,rb1) = nextSegment base
        (sa2,ra2) = nextSegment ra1
        (sb2,rb2) = nextSegment rb1

--  relPathFrom1 strips off trailing names from the supplied paths,
--  and calls difPathFrom to find the relative path from base to
--  target
relPathFrom1 :: String -> String -> String
relPathFrom1 pabs base = relName
    where
        (sa,na) = splitLast pabs
        (sb,nb) = splitLast base
        rp      = relSegsFrom sa sb
        relName = if null rp then
                      if (na == nb) then ""
                      else if protect na then "./"++na
                      else na
                  else
                      rp++na
        -- Precede name with some path if it is null or contains a ':'
        protect s = null s || ':' `elem` s

--  relSegsFrom discards any common leading segments from both paths,
--  then invokes difSegsFrom to calculate a relative path from the end
--  of the base path to the end of the target path.
--  The final name is handled separately, so this deals only with
--  "directory" segtments.
--
relSegsFrom :: String -> String -> String
{-
relSegsFrom sabs base
    | traceVal "\nrelSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
relSegsFrom []   []   = ""      -- paths are identical
relSegsFrom sabs base =
    if sa1 == sb1
        then relSegsFrom ra1 rb1
        else difSegsFrom sabs base
    where
        (sa1,ra1) = nextSegment sabs
        (sb1,rb1) = nextSegment base

--  difSegsFrom calculates a path difference from base to target,
--  not including the final name at the end of the path
--  (i.e. results always ends with '/')
--
--  This function operates under the invariant that the supplied
--  value of sabs is the desired path relative to the beginning of
--  base.  Thus, when base is empty, the desired path has been found.
--
difSegsFrom :: String -> String -> String
{-
difSegsFrom sabs base
    | traceVal "\ndifSegsFrom\nsabs " sabs $ traceVal "base " base $
      False = error ""
-}
difSegsFrom sabs ""   = sabs
difSegsFrom sabs base = difSegsFrom ("../"++sabs) (snd $ nextSegment base)

------------------------------------------------------------
--  Other normalization functions
------------------------------------------------------------

-- |Case normalization; cf. RFC3986 section 6.2.2.1
--  NOTE:  authority case normalization is not performed
--
normalizeCase :: String -> String
normalizeCase uristr = ncScheme uristr
    where
        ncScheme (':':cs)                = ':':ncEscape cs
        ncScheme (c:cs) | isSchemeChar c = toLower c:ncScheme cs
        ncScheme _                       = ncEscape uristr -- no scheme present
        ncEscape ('%':h1:h2:cs) = '%':toUpper h1:toUpper h2:ncEscape cs
        ncEscape (c:cs)         = c:ncEscape cs
        ncEscape []             = []

-- |Encoding normalization; cf. RFC3986 section 6.2.2.2
--
normalizeEscape :: String -> String
normalizeEscape ('%':h1:h2:cs)
    | isHexDigit h1 && isHexDigit h2 && isUnreserved escval =
        escval:normalizeEscape cs
    where
        escval = chr (digitToInt h1*16+digitToInt h2)
normalizeEscape (c:cs)         = c:normalizeEscape cs
normalizeEscape []             = []

-- |Path segment normalization; cf. RFC3986 section 6.2.2.4
--
normalizePathSegments :: String -> String
normalizePathSegments uristr = normstr juri
    where
        juri = parseURI uristr
        normstr Nothing  = uristr
        normstr (Just u) = show (normuri u)
        normuri u = u { uriPath = removeDotSegments (uriPath u) }

------------------------------------------------------------
--  Local trace helper functions
------------------------------------------------------------

traceShow :: Show a => String -> a -> a
traceShow msg x = trace (msg ++ show x) x

traceVal :: Show a => String -> a -> b -> b
traceVal msg x y = trace (msg ++ show x) y

------------------------------------------------------------
--  Deprecated functions
------------------------------------------------------------

{-# DEPRECATED parseabsoluteURI "use parseAbsoluteURI" #-}
parseabsoluteURI :: String -> Maybe URI
parseabsoluteURI = parseAbsoluteURI

{-# DEPRECATED escapeString "use escapeURIString, and note the flipped arguments" #-}
escapeString :: String -> (Char->Bool) -> String
escapeString = flip escapeURIString

{-# DEPRECATED reserved "use isReserved" #-}
reserved :: Char -> Bool
reserved = isReserved

{-# DEPRECATED unreserved "use isUnreserved" #-}
unreserved :: Char -> Bool
unreserved = isUnreserved

--  Additional component access functions for backward compatibility

{-# DEPRECATED scheme "use uriScheme" #-}
scheme :: URI -> String
scheme = orNull init . uriScheme

{-# DEPRECATED authority "use uriAuthority, and note changed functionality" #-}
authority :: URI -> String
authority = dropss . ($"") . uriAuthToString id . uriAuthority
    where
        -- Old-style authority component does not include leading '//'
        dropss ('/':'/':s) = s
        dropss s           = s

{-# DEPRECATED path "use uriPath" #-}
path :: URI -> String
path = uriPath

{-# DEPRECATED query "use uriQuery, and note changed functionality" #-}
query :: URI -> String
query = orNull tail . uriQuery

{-# DEPRECATED fragment "use uriFragment, and note changed functionality" #-}
fragment :: URI -> String
fragment = orNull tail . uriFragment

orNull :: ([a]->[a]) -> [a] -> [a]
orNull _ [] = []
orNull f as = f as

--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  Distributed as free software under the following license.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  - Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  - Neither name of the copyright holders nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDERS OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
--  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
--  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
--  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
