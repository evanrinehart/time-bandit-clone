{-# LANGUAGE DeriveFunctor #-}
module Clock where

import Animation
import Path

data Clock dt a = Clock !dt !dt a deriving (Functor, Show)

clock :: Num dt => A dt a -> A dt (Clock dt a)
clock go dt (Clock c rate y) = Clock (c + dt*rate) rate $! (go dt y)

clockT :: Clock dt a -> dt
clockT (Clock x _ _) = x

instance Body (Clock dt) where
  getBody (Clock _ _ x) = x

time :: Path (Clock dt a) dt
time = Path (w8 0) (\(Clock x _ _) -> Just x) (\f (Clock x y z) -> Clock (f x) y z)

rate :: Path (Clock dt a) dt
rate = Path (w8 1) (\(Clock _ y _) -> Just y) (\f (Clock x y z) -> Clock x (f y) z)
