{-# LANGUAGE DeriveFunctor #-}
module Newtonian where

-- a wrapper that adds uniform motion to an animation

import Animation
import Wrapper

data Newtonian a = Newtonian R2 R2 a deriving (Show, Functor)

instance Payload Newtonian where
  payload (Newtonian _ _ p) = p

onPosition :: (R2 -> R2) -> Newtonian a -> Newtonian a
onPosition f (Newtonian x v a) = (Newtonian (f x) v a)

onVelocity :: (R2 -> R2) -> Newtonian a -> Newtonian a
onVelocity f (Newtonian x v a) = (Newtonian x (f v) a)

position :: Newtonian a -> R2
position (Newtonian x _ _) = x

velocity :: Newtonian a -> R2
velocity (Newtonian _ v _) = v

newtonian :: R2 -> R2 -> A m v -> A (Newtonian m) (Newtonian v)
newtonian x0 v0 a0 = wrap (Newtonian x0 v0) f a0 where
  f _ go dt (Newtonian x v m) = (go dt m, Newtonian x' v) where
    x' = x .+. v .* realToFrac dt

infixl 6 .+.
(x,y) .+. (z,w) = (x+z, y+w)
(x,y) .* r = (r*x, r*y)
(*.) = flip (.*)
