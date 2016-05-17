{-# LANGUAGE DeriveFunctor #-}
module Physics where

-- a wrapper that regulates the passage of time

import Animation
import Collection
import Wrapper

data UniformTime a = UniformTime
  { stepPeriod :: Delta
  , stepCounter :: Time
  , body :: a
  } deriving (Show, Functor)

instance Payload UniformTime where
  payload = body

uniformTime :: Delta -> A m v -> A (UniformTime m) v
uniformTime period0 a0 = fmap body $ wrap (UniformTime period0 0) f a0 where
  f a go dt (UniformTime period c m) = (a', UniformTime period c'') where
    c' = c + dt
    (n, c'') = fdivmod c' period
    a' = iterateN n (advance period) (modify (const m) a)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN i f x | i > 0 = iterateN (i-1) f (f x)
               | otherwise = error ("iterateN where n="++show i)

fdivmod :: (RealFrac a, Integral b) => a -> a -> (b, a)
fdivmod x y = let n = floor (x / y) in (n, x - realToFrac n * y)
