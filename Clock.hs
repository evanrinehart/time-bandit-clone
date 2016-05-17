module Clock where

-- a number that increases with time according to a specified rate

import Animation

data Clock = Clock Time Rate deriving Show

time :: Clock -> Time
time (Clock t _) = t

clock :: Time -> Rate -> A Clock Time
clock t0 speed0 = (fmap time . anim shift) (Clock t0 speed0)

shift :: Delta -> Clock -> Clock
shift dt (Clock t rate) = Clock (t + dt*rate) rate

onTime :: (Time -> Time) -> Clock -> Clock
onTime f (Clock a b) = Clock (f a) b

onSpeed :: (Rate -> Rate) -> Clock -> Clock
onSpeed f (Clock a b) = Clock a (f b)
