{-# LANGUAGE DeriveFunctor #-}
module Toroidal where

-- when wrapped directly on a newtonian animation, causes position to
-- wrap around when it exits the specified bounds

import Animation
import Newtonian
import Wrapper

data Toroidal a = Toroidal R R R R a deriving (Show, Functor)

instance Payload Toroidal where
  payload (Toroidal _ _ _ _ a) = a

toroidal ::
  R -> R -> R -> R ->
  A (Newtonian m) v ->
  A (Toroidal (Newtonian m)) v
toroidal l r b t a0 = fmap payload $ wrap (Toroidal l r b t) f a0 where
  f _ go dt (Toroidal l r b t m) = (a', Toroidal l r b t) where
    a' = modify (onPosition edit) (go dt m)
    edit = (\(x,y) -> (torus x l r, torus y b t))

torus :: R -> R -> R -> R
torus x l r | x >= r = torus (x - (r - l)) l r
            | x < l = torus (x + (r - l)) l r
            | otherwise = x
