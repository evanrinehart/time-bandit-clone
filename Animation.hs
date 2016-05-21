{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Animation where

import Data.Fixed
import Data.Map.Strict as M
import Data.Set as S
import Data.Bifunctor
import Data.Monoid
import Control.Applicative (liftA2)

data A dt a = A !a (dt -> a -> a)

type T = Pico
type Delta = T
type Time = T
type Rate = T
type R = Double
type R2 = (R,R)

type Animation a = A Delta a
type Go dt a = dt -> a -> a

sample :: A dt a -> a
sample (A x _) = x

advance :: dt -> A dt a -> A dt a
advance dt (A x go) = A (go dt x) go

modify :: (a -> a) -> A dt a -> A dt a
modify f (A x go) = A (f x) go

isoMap :: (a -> b) -> (b -> a) -> Go dt a -> Go dt b
isoMap f g go dt y = f (go dt (g y))

anim :: a -> Go dt a -> A dt a
anim = A

blank :: Go dt ()
blank _ _ = ()

instance (Show a) => Show (A dt a) where
  show (A x _) = show x

wrap :: Functor f => Go dt a -> Go dt (f a)
wrap = (fmap .)

fuse :: Bifunctor f => Go dt a -> Go dt b -> Go dt (f a b)
fuse = liftA2 bimap
