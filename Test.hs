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

m0 :: Model
m0 =
  ( (False, AirRunningOut (Clock 10 (-1) ()))
  , UniformTime 0 (Phys1 (-60,-60) (0,0) ((* 10) . sombrero) (0,0) ()))

animation :: A Pico Model
animation = fuse (fuse (const id) air) (uniformTime 0.0001 (phys1 (const id)))

mySim0 = newSimulation m0 animation
  [ outOfAirRule (first >>> second >>> airTimeLeft)
  , planetRule (first >>> first) (first >>> second) (second >>> body)
  , spaceRule]
  []

control :: R2 -> Bool -> Poke Model ()
control dir False = set (second >>> body >>> force) (0,0)
control dir True = set (second >>> body >>> force) dir

outOfAirRule :: Path Model Delta -> Rule Delta Model
outOfAirRule airTimeLeft = Rule "out-of-air" $ do
  t <- view airTimeLeft
  planTo_ t (execIO exitSuccess)

planetRule :: Path Model Bool
           -> Path Model (Air Delta)
           -> Path Model (Phys1 a)
           -> Rule Delta Model
planetRule onPlanet air ship = Rule "enter-planet" $ do
  require onPlanet
  predictEntry <$> view ship >>= \case
    Left dt -> planToWait dt
    Right dt -> planTo_ dt $ do
      set onPlanet True
      set air HaveAir

spaceRule :: Rule Delta Model
spaceRule = Rule "leave-planet" $ do
  onPlanet <- view (first >>> first)
  when (not onPlanet) (fail "already in space")
  ship <- view (second >>> body)
  case predictExit ship of
    Left dt -> return (dt, return ())
    Right dt -> return $ (dt,) $ do
      set (first >>> first) False
      set (first >>> second) (AirRunningOut (Clock 10 (-1) ()))

getAngle :: R2 -> R2 -> R
getAngle u v | norm u == 0 || norm v == 0 = 0
             | otherwise =
  let (x1,x2) = unitv u in
  let (y1,y2) = unitv v in
  let alpha = acos (x1*y1 + x2*y2) in
  let beta  = asin (x1*y2 - x2*y1) in
  if signum beta == 0 then alpha else alpha * signum beta

dot :: R2 -> R2 -> R
dot (a,b) (c,d) = a*c + b*d

unitv :: R2 -> R2
unitv (x,y) = (x / norm (x,y), y / norm (x,y)) 

norm (x,y) = sqrt (x*x + y*y)

radToDeg rad = 180*rad/pi
