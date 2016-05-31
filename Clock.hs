{-# LANGUAGE DeriveFunctor #-}
module Clock where

import Animation
import Path

data Clock dt = Clock !dt !dt deriving (Show)

clock :: Num dt => A dt (Clock dt)
clock dt (Clock c rate) = Clock (c + dt*rate) rate 

time :: Path (Clock dt) dt
time = Path (w8 0) (\(Clock x _) -> Just x) (\f (Clock x y) -> Clock (f x) y)

rate :: Path (Clock dt) dt
rate = Path (w8 1) (\(Clock y _) -> Just y) (\f (Clock x y) -> Clock x (f y))
