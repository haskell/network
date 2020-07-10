{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}


module Network.Socket.ReadShow where

import qualified Text.Read as P

-- type alias for individual correspondences of a (possibly partial) bijection
type Pair a b = (a, b)

-- | helper function for equality on first tuple element
{-# INLINE eqFst #-}
eqFst :: Eq a => a -> (a, b) -> Bool
eqFst x = \(x',_) -> x' == x

-- | helper function for equality on snd tuple element
{-# INLINE eqSnd #-}
eqSnd :: Eq b => b -> (a, b) -> Bool
eqSnd y = \(_,y') -> y' == y

-- | Return RHS element that is paired with provided LHS,
--   or apply a default fallback function if the list is partial
lookForward :: Eq a => (a -> b) -> [Pair a b] -> a -> b
lookForward defFwd ps x
  = case filter (eqFst x) ps of
      (_,y):_ -> y
      [] -> defFwd x

-- | Return LHS element that is paired with provided RHS,
--   or apply a default fallback function if the list is partial
lookBackward :: Eq b => (b -> a) -> [Pair a b] -> b -> a
lookBackward defBwd ps y
  = case filter (eqSnd y) ps of
      (x,_):_ -> x
      [] -> defBwd y

data Bijection a b
   = Bijection
     { defFwd :: a -> b
     , defBwd :: b -> a
     , pairs  :: [Pair a b]
     }

-- | apply a bijection over an LHS-value
forward :: (Eq a) => Bijection a b -> a -> b
forward Bijection{..} = lookForward defFwd pairs

-- | apply a bijection over an RHS-value
backward :: (Eq b) => Bijection a b -> b -> a
backward Bijection{..} = lookBackward defBwd pairs

-- | show function for Int-like types that encodes negative numbers
-- with leading '_' instead of '-'
_showInt :: (Show a, Num a, Ord a) => a -> String
_showInt n | n < 0 = let ('-':s) = show n in '_':s
           | otherwise = show n

-- | parse function for Int-like types that interprets leading '_'
--   as if it were '-' instead
_readInt :: (Read a) => String -> a
_readInt ('_':s) = read $ '-':s
_readInt s = read s


-- | parse a quote-separated pair into a tuple of Int-like values
--   should not be used if either type might have
--   literal quote-characters in the Read pre-image
_parse :: (Read a, Read b) => String -> (a, b)
_parse xy =
  let (xs, '\'':ys) = break (=='\'') xy
   in (_readInt xs, _readInt ys)
{-# INLINE _parse #-}

-- | inverse function to _parse
--   show a tuple of Int-like values as quote-separated strings
_show :: (Show a, Num a, Ord a,  Show b, Num b, Ord b) => (a, b) -> String
_show (x, y) = _showInt x ++ "'" ++ _showInt y
{-# INLINE _show #-}

defShow :: Eq a => String -> (a -> b) -> (b -> String) -> (a -> String)
defShow name unwrap sho = \x -> name ++ (sho . unwrap $ x)
{-# INLINE defShow #-}

defRead :: Read a => String -> (b -> a) -> (String -> b) -> (String -> a)
defRead name wrap red = \s ->
  case splitAt (length name) s of
    (x, sn) | x == name -> wrap $ red sn
    _ -> error $ "defRead: unable to parse " ++ show s
{-# INLINE defRead #-}

-- | Apply a precedence-invariant one-token parse function within ReadPrec monad
tokenize :: (String -> a) -> P.ReadPrec a
tokenize f = P.lexP >>= \(P.Ident x) -> return $ f x
{-# INLINE tokenize #-}
