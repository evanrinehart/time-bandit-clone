module Bag where

data Bag a = Empty | Single a | Concat (Bag a) (Bag a) deriving (Show,Eq,Ord)

single :: a -> Bag a
single = Single

toList :: Bag a -> [a]
toList b = go [] b where
  go xs Empty = xs
  go xs (Single x) = x : xs
  go xs (Concat b1 b2) = go (go xs b1) b2

instance Monoid (Bag a) where
  mempty = Empty
  mappend = Concat
