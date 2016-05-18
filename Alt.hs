{-# LANGUAGE DeriveFunctor #-}
module Alt where

-- a wrapper which contains two simulations but only one is running at a time

import Data.Bifunctor
import Animation
import Wrapper
import Path
import Bag

data Alt a b = Alt1 a b | Alt2 a b deriving (Show)

instance Bifunctor Alt where
  bimap f g (Alt1 x y) = Alt1 (f x) (g y)
  bimap f g (Alt2 x y) = Alt2 (f x) (g y)

toggle :: Alt a b -> Alt a b
toggle (Alt1 x y) = Alt2 x y
toggle (Alt2 x y) = Alt1 x y

alt1 :: Path (Alt a b) a
alt1 = Path (single 0) g e where
  g (Alt1 x _) = x
  g (Alt2 x _) = x
  e f (Alt1 x y) = Alt1 (f x) y
  e f (Alt2 x y) = Alt2 (f x) y

alt2 :: Path (Alt a b) b
alt2 = Path (single 0) g e where
  g (Alt1 _ y) = y
  g (Alt2 _ y) = y
  e f (Alt1 x y) = Alt1 x (f y)
  e f (Alt2 x y) = Alt2 x (f y)

alt :: A m1 v1 -> A m2 v2 -> A (Alt m1 m2) (Alt v1 v2)
alt a1@(A m1 v1 go1) a2@(A m2 v2 go2) =
  A (Alt1 m1 m2) (bimap v1 v2) (go a1 a2) where
    go a1 a2 dt (Alt1 m1 m2) =
      A (Alt1 m1' m2) (bimap v1' v2) (go a1' a2) where
        A _ v1 go1 = modify (const m1) a1
        A _ v2 go2 = modify (const m2) a2
        a1'@(A m1' v1' go1') = go1 dt m1
    go a1 a2 dt (Alt2 m1 m2) =
      A (Alt2 m1 m2') (bimap v1 v2') (go a1 a2') where
        A _ v1 go1 = modify (const m1) a1
        A _ v2 go2 = modify (const m2) a2
        a2'@(A m2' v2' go2') = go2 dt m2
