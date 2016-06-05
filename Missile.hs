module Missile where

import Motion
import Clock
import Animation

type MissileType = Int
data Missile = Missile !(Clock Double) !MissileType deriving Show

timeLeft :: Missile -> Double
timeLeft (Missile (Clock c rate) _) = c/rate

mkMissile :: MissileType -> Missile
mkMissile ty = Missile (Clock 0.5 (-1)) ty

missile :: A Double Missile
missile dt (Missile cl mty) = Missile (clock dt cl) mty
