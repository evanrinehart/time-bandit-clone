{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Test where

import System.Exit
import Data.Fixed
import Prelude hiding ((.),id)
import Control.Category
import Control.Monad
import Animation
import Algorithm
import Phys1
import UniformTime
import Types
import Poke
import Path
import Air
import Clock
import Viewing

type Model =
  ((Bool, Air Delta), UniformTime Delta (Phys1 ()))
type Iface = Interface Time Model
type Rule' = Rule Delta Model
type PathTo a = Path Model a
type Ship = Phys1 ()

m0 :: Model
m0 =
  ( (False, AirRunningOut (Clock 10 (-1) ()))
  , UniformTime 0 (Phys1 (-60,-60) (0,0) ((* 10) . sombrero) (0,0) ()))

animation :: A Pico Model
animation = fuse (fuse (const id) air) (uniformTime 0.0001 (phys1 (const id)))

mySim0 = newSimulation m0 animation
  [ outOfAirRule (first >>> second >>> airTimeLeft)
  , planetRule (first >>> first) (first >>> second) (second >>> body)
  , spaceRule (first >>> first) (first >>> second) (second >>> body)
  ]
  []

control :: R2 -> Bool -> Poke Model ()
control dir False = set (second >>> body >>> force) (0,0)
control dir True = set (second >>> body >>> force) dir

outOfAirRule :: PathTo Delta -> Rule'
outOfAirRule airTimeLeft = Rule "out-of-air" $ do
  t <- view airTimeLeft
  planTo_ t (execIO exitSuccess)

planetRule :: PathTo Bool -> PathTo (Air Delta) -> PathTo Ship -> Rule'
planetRule onPlanet air ship = Rule "enter-planet" $ do
  require onPlanet
  predictEntry <$> view ship >>= \case
    Left dt -> planToWait dt
    Right dt -> planTo_ dt $ do
      set onPlanet True
      set air HaveAir

spaceRule :: PathTo Bool -> PathTo (Air Delta) -> PathTo Ship -> Rule'
spaceRule onPlanet air ship = Rule "leave-planet" $ do
  requireThat not onPlanet
  predictExit <$> view ship >>= \case
    Left dt -> planToWait dt
    Right dt -> planTo_ dt $ do
      set onPlanet False
      set air (AirRunningOut (Clock 10 (-1) ()))

