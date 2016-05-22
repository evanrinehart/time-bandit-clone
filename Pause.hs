{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Pause where

import Animation
import Path

data Pause a = Paused a | Unpaused a deriving (Show, Functor)

toggle :: Pause a -> Pause a
toggle (Paused x) = Unpaused x
toggle (Unpaused x) = Paused x

update :: (a -> b) -> Pause a -> Pause b
update f (Paused x) = Paused (f x)
update f (Unpaused x) = Unpaused (f x)

instance Body Pause where
  getBody (Paused x) = x
  getBody (Unpaused x) = x

pausable :: A dt a -> A dt (Pause a)
pausable go dt = \case
  Paused x -> Paused x
  Unpaused x -> Unpaused $! go dt x
