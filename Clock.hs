module Clock where

import Animation
import Path

data Clock dt = Clock !dt !dt deriving Show

clock :: Num dt => dt -> dt -> A dt (Clock dt)
clock c0 rate0 = A (Clock c0 rate0) go where
  go dt (Clock c0 rate0) = Clock (c0 + dt*rate0) rate0

time :: Path (Clock dt) dt
time = Path (w8 0) (\(Clock x _) -> Just x) (\f (Clock x y) -> Clock (f x) y)

rate :: Path (Clock dt) dt
rate = Path (w8 1) (\(Clock _ y) -> Just y) (\f (Clock x y) -> Clock x (f y))
