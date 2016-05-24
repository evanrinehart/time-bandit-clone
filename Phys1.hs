{-# LANGUAGE DeriveFunctor #-}
module Phys1 where

-- a wrapper that adds uniform motion to an animation
-- also includes optional uniform acceleration, featuring a crap integrator

import Animation
import Path
import Types

import Data.Fixed

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

-- when will x enter the circle with radius 10, or give up
predictEntry :: Phys1 a -> Either Pico Pico
predictEntry (Phys1 x0 v0 u f pay) = go 1 x0 v0 where
  go 0 x0 v0 = Left 1
  go timeLeft x0 v0 =
    let r = 25 in
    let dt = 0.0001 in
    let x1 = x0 .+. v0 .* dt in
    let a = negateR2 (gradient u x0) .+. f in
    let v1 = v0 .+. a .* dt in
    if lineCrossCircle x0 x1 r && outside x0 r && inside x1 r
      then let crossTime = solveLine x0 x1 0 dt (\x -> lineCrossCircle x0 x 25) in
           Right (realToFrac (1 - timeLeft + crossTime))
      else go (max 0 (timeLeft - dt)) x1 v1

-- when will x leave the circle with radius 10, or give up
predictExit :: Phys1 a -> Either Pico Pico
predictExit (Phys1 x0 v0 u f pay) = go 1 x0 v0 where
  go 0 x0 v0 = Left 1
  go timeLeft x0 v0 =
    let r = 25 in
    let dt = 0.0001 in
    let x1 = x0 .+. v0 .* dt in
    let a = negateR2 (gradient u x0) .+. f in
    let v1 = v0 .+. a .* dt in
    if lineCrossCircle x0 x1 r && outside x1 r && inside x0 r
      then let crossTime = solveLine x0 x1 0 dt (\x -> lineCrossCircle x0 x 25) in
           Right (realToFrac (1 - timeLeft + crossTime))
      else go (max 0 (timeLeft - dt)) x1 v1

outside :: R2 -> R -> Bool
outside (x,y) r = sqrt (x**2 + y**2) > r

inside :: R2 -> R -> Bool
inside (x,y) r = sqrt (x**2 + y**2) < r

lineCrossCircle :: R2 -> R2 -> R -> Bool
lineCrossCircle (x1,y1) (x2,y2) r = this || that where
  this = r1 > r && r2 < r
  that = r1 < r && r2 > r
  r1 = sqrt (x1**2 + y1**2)
  r2 = sqrt (x2**2 + y2**2)

-- exactly when does a condition go from false to true along this linear path
solveLine :: R2 -> R2 -> R -> R -> (R2 -> Bool) -> R
solveLine x0 v t0 t1 check = go t0 ((t0 + t1) / 2) where
  go tprev t | t - tprev < 1e-9 = t
             | check (x0 .+. v .* (t - t0)) = go t ((t0 + t) / 2)
             | otherwise = go t ((t + t1) / 2)
