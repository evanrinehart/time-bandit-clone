{-# LANGUAGE DeriveFunctor #-}
module Rotating where

-- a wrapper that adds a direction and speed of rotation to an animation

import Animation
import Path
import Types

data Rotating a = Rotating R2 R a deriving (Show, Functor)

rotation :: Path (Rotating a) R2
rotation (Rotating r _ _) = r

rotvel :: Path (Rotating a) R2
rotvel (Rotating _ v _) = v

instance Payload Rotating where
  payload (Rotating _ _ a) = a

rotating :: R2 -> R -> A m v -> A (Rotating m) (Rotating v)
rotating d0 v0 a0 = wrap (Rotating d0 v0) f a0 where
  f _ go dt (Rotating d v m) = (go dt m, Rotating d' v) where
    d' = rotate (v * realToFrac dt) d

rotate :: R -> R2 -> R2
rotate a (x,y) = (x*cos a - y*sin a, x*sin a + y*cos a)

{-
[cos t  -sin t  [x
 sin t   cos t]  y]  = (x cos t - y sin t, x sint + y cos t)
-}
