{-# LANGUAGE DeriveFunctor #-}
module Newtonian where

-- a wrapper that adds uniform motion to an animation
-- also includes optional uniform acceleration, featuring a crap integrator

import Animation
import Wrapper

data Newtonian a = Newtonian
  { position :: R2
  , velocity :: R2
  , acceleration :: R2
  , body :: a
  } deriving (Show, Functor)

instance Payload Newtonian where
  payload = body

onPosition :: (R2 -> R2) -> Newtonian a -> Newtonian a
onPosition f (Newtonian x v a p) = (Newtonian (f x) v a p)

onVelocity :: (R2 -> R2) -> Newtonian a -> Newtonian a
onVelocity f (Newtonian x v a p) = (Newtonian x (f v) a p)

onAcceleration :: (R2 -> R2) -> Newtonian a -> Newtonian a
onAcceleration f (Newtonian x v a p) = (Newtonian x v (f a) p)

newtonian :: R2 -> R2 -> R2 -> A m v -> A (Newtonian m) (Newtonian v)
newtonian x0 v0 a0 p0 = wrap (Newtonian x0 v0 a0) f p0 where
  f _ go dt (Newtonian x v a m) = (go dt m, Newtonian x' v' a) where
    v' = v .+. a .* realToFrac dt
    x' = x .+. v' .* realToFrac dt

infixl 6 .+.
(x,y) .+. (z,w) = (x+z, y+w)
(x,y) .* r = (r*x, r*y)
(*.) = flip (.*)
