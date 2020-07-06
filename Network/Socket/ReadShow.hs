{-# LANGUAGE RecordWildCards #-}

module Network.Socket.ReadShow where

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

-- | parse an underscore-separated pair into a tuple
--   should not be used if either type might have
--   literal underscores in the Read pre-image
_parse :: (Read a, Read b) => String -> (a, b)
_parse xy =
  let (xs, '_':ys) = break (=='_') xy
   in (read xs, read ys)

