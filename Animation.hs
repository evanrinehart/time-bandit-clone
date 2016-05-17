module Animation where

import Data.Fixed

-- A m v is a modifiable animation which consists of
-- m - the model which can be modified
-- m -> v - the image of the model of type v (view)
-- Delta -> m -> A m v - time-mutation, this algorithm ideally obeys a
-- determinism law which lets a given A m v be interpreted as a continuous
-- flow of m and v through time. (for any sequence of Deltas, the iterated
-- sequence of m and v are consistent for any chosen t in time)

data A m v = A m (m -> v) (Delta -> m -> A m v)

type T = Pico
type Delta = T
type Time = T
type Rate = T
type R = Double
type R2 = (R,R)

type Blank = A () ()

view :: A m v -> v
view (A m view _) = view m

model :: A m v -> m
model (A m _ _) = m

advance :: Delta -> A m v -> A m v
advance dt (A m view go) = go dt m

modify :: (m -> m) -> A m v -> A m v
modify f (A m view go) = A (f m) view go

mapView :: (u -> v) -> A m u -> A m v
mapView f (A m view go0) = A m (fmap f view) (go go0) where
  go innerGo m dt =
    let A m' view' go' = innerGo m dt in
    A m' (fmap f view') (go go')

isoModel :: (m -> n) -> (n -> m) -> A m v -> A n v
isoModel f g (A m view go0) = A (f m) (view . g) (go go0) where
  go innerGo dt n =
    let A m' view' go' = innerGo dt (g n) in
    A (f m') (view' . g) (go go')

anim :: (Delta -> m -> m) -> m -> A m m
anim f m0 = A m0 id go where
  go dt m = A (f dt m) id go
    
still :: v -> A v v
still x = anim (const id) x

blank :: A () ()
blank = A () id (\_ _ -> blank)

instance Functor (A m) where
  fmap = mapView

instance (Show m, Show v) => Show (A m v) where
  show (A x view advance) = show x ++ " >- " ++ show (view x)

x & f = f x

class Functor f => Payload f where
  payload :: f a -> a
