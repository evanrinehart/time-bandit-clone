module Facade where

import Control.Monad
import Control.Monad.State
import Path
import Poke
import Viewing
import Animation

data Facade s v = Facade
  { facView :: s -> v
  , facVEq :: v -> v -> Bool
  , facModel :: s
  }

instance (Show s, Show v) => Show (Facade s v) where
  showsPrec d (Facade v _ s) =
    showParen (d > 10) $
    showString "Facade " .
    showsPrec 11 (v s) . 
    showString " " .
    showsPrec 11 s

facade :: A dt s -> A dt (Facade s a)
facade f dt (Facade x y z) = Facade x y (f dt z)

_facade :: Path (Facade s v) v
_facade = Path (w8 0) get modify where
  get (Facade image eq s) = Just (image s)
  modify _ orig = orig -- you can't modify the view

-- modify the internal data of the facade. if the view does not change, then
-- the update does not count as modifying the facade. plans cannot read the
-- internal state, only the facade's view.
updateInternal :: Path m (Facade s v) -> (s -> s) -> Poke m ()
updateInternal path f = do
  Facade image veq model <- view path
  let model' = f model
  let v0 = image model
  let v1 = image model'
  s <- Poke get
  let s' = pathUpdate path (const (Facade image veq model')) s
  -- directly update the data, conditionally register the modification
  Poke (put s')
  when (not (v0 `veq` v1)) (touch path)
