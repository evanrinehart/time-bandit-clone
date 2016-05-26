module Cyclic where

import Data.Fixed
import Animation
import Path

data Cyclic a = Cyclic Pico Pico Int [a]

mkCyclic :: Pico -> [a] -> Cyclic a
mkCyclic p [] = error "empty cyclic animation"
mkCyclic p xs = Cyclic p 0 (length xs) (cycle xs)

uncyclic :: Cyclic a -> a
uncyclic (Cyclic _ _ _ (x:xs)) = x

cyclic :: A Pico (Cyclic a)
cyclic dt (Cyclic p c len xs) = Cyclic p c' len xs' where
  (n, c') = divMod' (c + dt) p
  xs' = drop n xs

cyclicRate :: Path (Cyclic a) Pico
cyclicRate = Path (w8 0) g s where
  g (Cyclic x _ _ _) = Just x
  s f (Cyclic x y l z) = Cyclic (f x) y l z

instance Show a => Show (Cyclic a) where
  show (Cyclic p c l xs) = "Cyclic " ++ show p ++ show c ++ show (take l xs)
