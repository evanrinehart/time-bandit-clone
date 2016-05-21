{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Animation where

import Data.Fixed
import Data.Map as M
import Data.Set as S

data A dt a = A a (dt -> a -> a)

type T = Pico
type Delta = T
type Time = T
type Rate = T
type R = Double
type R2 = (R,R)

type Animation a = A Delta a
type Go dt a = dt -> a -> a
type Mapper1 f = forall a . (a -> a) -> f a -> f a

sample :: A dt a -> a
sample (A x _) = x

advance :: dt -> A dt a -> A dt a
advance dt (A x go) = A (go dt x) go

modify :: (a -> a) -> A dt a -> A dt a
modify f (A x go) = A (f x) go

isoMap :: (a -> b) -> (b -> a) -> A dt a -> A dt b
isoMap f g (A x go) = A (f x) go' where
  go' dt y = f (go dt (g y))

still :: a -> A dt a
still x = A x (const id)

instance (Show a) => Show (A dt a) where
  show (A x _) = show x 

wrap1 :: f a -> ((a -> a) -> f a -> f a) -> (dt -> a -> a) -> A dt (f a)
wrap1 x0 map go = A x0 go' where
  go' dt x = map (go dt) x

barn :: Ord k => [(k,a)] -> (dt -> a -> a) -> A dt (Map k a)
barn kv0 go = wrap1 (M.fromList kv0) M.map go

ability :: (a -> f a) -> (dt -> (dt -> a -> a) -> f a -> f a) -> A dt a -> A dt (f a)
ability mk go' (A x go) = A (mk x) go'' where
  go'' dt y = go' dt go y

