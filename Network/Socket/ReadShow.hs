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

forward :: (Eq a) => Bijection a b -> a -> b
forward Bijection{..} = lookForward defFwd pairs

backward :: (Eq b) => Bijection a b -> b -> a
backward Bijection{..} = lookBackward defBwd pairs
