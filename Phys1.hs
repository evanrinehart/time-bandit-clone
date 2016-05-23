{-# LANGUAGE DeriveFunctor #-}
module Phys1 where

-- a wrapper that adds uniform motion to an animation
-- also includes optional uniform acceleration, featuring a crap integrator

import Animation
import Path
import Types

data Phys1 a = Phys1
  { physX :: R2
  , physV :: R2
  , physU :: R2 -> R
  , physF :: R2
  , physBody :: a
  } deriving (Functor)

instance Body Phys1 where
  getBody = physBody

instance Show a => Show (Phys1 a) where
  show (Phys1 x v _ f a) = "Phys1 " ++ show x ++ " " ++ show v ++ " " ++ show f ++ " " ++ show a

gradient :: (R2 -> R) -> R2 -> R2
gradient u (x,y) = ((dux2 - dux1)/delta, (duy2 - duy1)/delta) where
  delta = 0.001
  dux2 = u (x+delta, y)
  dux1 = u (x-delta, y)
  duy2 = u (x, y+delta)
  duy1 = u (x, y-delta)

sombrero :: R2 -> R
sombrero (x,y) = (10000 / (1 + r**2)) + (0.01 * r**2) where
  r = sqrt (x*x + y*y)
 
position :: Path (Phys1 a) R2
position = Path (w8 0) (Just . physX) (\f (Phys1 x y z w u) -> Phys1 (f x) y z w u)

velocity :: Path (Phys1 a) R2
velocity = Path (w8 1) (Just . physV) (\f (Phys1 x y z w u) -> Phys1 x (f y) z w u)

potential :: Path (Phys1 a) (R2 -> R)
potential = Path (w8 2) (Just . physU) (\f (Phys1 x y z w u) -> Phys1 x y (f z) w u)

force :: Path (Phys1 a) R2
force = Path (w8 3) (Just . physF) (\f (Phys1 x y z w u) -> Phys1 x y z (f w) u)

phys1 :: RealFrac dt => A dt a -> A dt (Phys1 a)
phys1 go dt (Phys1 x v u f content) = Phys1 x' v' u f content where
  x' = x .+. v .* (realToFrac dt)
  v' = v .+. a .* (realToFrac dt)
  a = negateR2 (gradient u x) .+. f

infixl 6 .+.
(x,y) .+. (z,w) = (x+z, y+w)
(x,y) .* r = (r*x, r*y)
(*.) = flip (.*)
negateR2 (x,y) = (-x,-y)
