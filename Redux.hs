{-# LANGUAGE DeriveFunctor #-}
module Redux where

data A dt s = A s (dt -> A dt s) deriving Functor

instance Show s => Show (A dt s) where
  showsPrec d (A x _) =
    showParen (d > 10) $
    showString "A " .
    showsPrec 11 x

class Bisect a where
  bisect :: a -> a -> a

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

data Pause a = Pause a | Unpause a

pausable :: A dt a -> A dt (Pause a)
pausable (A s0 f) = sim (Pause s0) g where
  g dt (Pause x) = Pause x
  g dt (Unpause x) = Unpause (f dt x)



