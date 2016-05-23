module Test where

import Data.Fixed
import Prelude hiding ((.),id)
import Control.Category
import Animation
import Algorithm
import Phys1
import UniformTime
import Types
import Poke
import Path

type Model = UniformTime Delta (Phys1 ())
type Iface = Interface Time Model

m0 :: Model
m0 = UniformTime 0 (Phys1 (-50,-50) (0,1) ((* 10) . sombrero) (0,0) ())

animation :: A Pico Model
animation = uniformTime 0.0001 (phys1 (const id))

control :: R2 -> Bool -> Poke Model ()
control dir False = update (body >>> force) (const (0,0))
control dir True = update (body >>> force) (const dir)

  

getAngle :: R2 -> R2 -> R
getAngle u v =
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
