module Cooldown where

import Animation
import Path

data Cooldown = Ready | Cooldown Double deriving Show

cooldown :: Real dt => A dt Cooldown
cooldown dt Ready = Ready
cooldown dt (Cooldown t)
  | (realToFrac dt) < t = Cooldown (t - (realToFrac dt))
  | otherwise = Ready

timeLeft :: Path Cooldown Double
timeLeft = Path (w8 1) g s where
  g Ready = Nothing
  g (Cooldown t) = Just t
  s _ Ready = Ready
  s f (Cooldown t) = Cooldown (f t)
