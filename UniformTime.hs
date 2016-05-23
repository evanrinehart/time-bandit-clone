{-# LANGUAGE DeriveFunctor #-}
module UniformTime where

-- a wrapper that regulates the passage of time

import Animation
import Path
import Types
import Data.Fixed

data UniformTime dt a = UniformTime
  { uniformCounter :: dt
  , uniformBody :: a
  } deriving (Show, Functor)

instance Body (UniformTime dt) where
  getBody = uniformBody

uniformTime :: Real dt => dt -> A dt a -> A dt (UniformTime dt a)
uniformTime period go dt (UniformTime c y) = UniformTime c'' y' where
    c' = c + dt
    (n, c'') = divMod' c' period
    y' = iterateN n (go period) y

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN i f x | i > 0 = iterateN (i-1) f (f x)
               | otherwise = error ("iterateN where n="++show i)

