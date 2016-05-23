{-# LANGUAGE DeriveFunctor #-}
module Shared where

import Animation
import Path

data Shared s a = Shared
  { sharedModel :: s
  , sharedBody :: a
  } deriving (Show, Functor)

instance Body (Shared s) where
  getBody = sharedBody

shared :: Path (Shared s a) s
shared = Path (w8 0) (Just . sharedModel) (\f (Shared x y) -> Shared (f x) y)

share :: A dt s -> (s -> A dt s') -> A dt (Shared s s')
share go f dt (Shared x y) = (Shared x' y') where
  x' = go dt x
  y' = f x' dt y
