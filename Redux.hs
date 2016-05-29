{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module Redux where

import Control.Applicative

data A dt s = A s (dt -> A dt s) deriving Functor

instance Show s => Show (A dt s) where
  showsPrec d (A x _) =
    showParen (d > 10) $
    showString "A " .
    showsPrec 11 x

class Bisect a where
  bisect :: a -> a -> a

instance Applicative (A dt) where
  pure = still
  af <*> ax = fmap f $ sim (af,ax) g where
    f (af,ax) = (sample af) (sample ax)
    g dt (af,ax) = (shift dt af, shift dt ax)

instance Bisect Double where
  bisect x y = if b*2 == x+y then b else error "unbisectable" where
    b = (x+y)/2

sample :: A dt s -> s
sample (A x _) = x

shift :: dt -> A dt s -> A dt s
shift dt (A x f) = f dt

still :: s -> A dt s
still x = sim x (const id)

func :: Num dt => (dt -> s) -> A dt s
func f = A (f 0) (\dt -> func (f . (+dt)))

sim :: m -> (dt -> m -> m) -> A dt m
sim m0 f = A m0 (\dt -> sim (f dt m0) f)

missile :: Double -> Double -> A Double (Double, Double)
missile x0 v0 = sim (x0,v0) (\dt (x,v) -> (x+v*dt, v))

clock = fmap fst (missile 0 1)

data Pause a = Pause a | Unpause a deriving Show

unpause (Pause x) = x
unpause (Unpause x) = x

pausable :: (forall a . a -> Pause a) -> A dt a -> A dt (Pause a)
pausable mk a = fmap (mk . sample . unpause) $ sim (mk a) g where
  g dt (Pause a) = Pause a
  g dt (Unpause a) = Unpause (shift dt a)

toggle (Pause x) = Unpause x
toggle (Unpause x) = Pause x

unpaz :: A dt (Pause a) -> A dt (Pause a)
unpaz (A (Pause a) _) = pausable Unpause a
unpaz (A (Unpause a) _) = pausable Pause a

fuse :: (a -> b -> c) -> A dt a -> A dt b -> A dt c
fuse = liftA2

myA :: (Int, Pause (Int, (Double,Double)))
    -> A Double (Int, Pause (Int, (Double,Double)))
myA m0 = fuse (,) (still 3) (pausable Unpause (fuse (,) (still 1) (missile 3 4)))
