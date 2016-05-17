module Asteroids where

-- an asteroids ship animation

import Animation
import Newtonian
import Rotating
import Toroidal
import Physics

data ShipImage = ShipImage
  { shipX :: R2
  , shipDir :: R2 }
    deriving (Show)

ship :: R2 -> R2 -> A ShipGuts ShipImage
ship x0 v0 = ship' x0 v0 blank

ship' :: R2 -> R2 -> Blank -> A ShipGuts ShipImage
ship' x0 v0 = 
  fmap (ShipImage . position . payload <*> rotation) .
  uniformTime 0.01 .
  rotating (0,1) 0 .
  toroidal 0 640 0 480 .
  newtonian x0 v0 (0,0)

type ShipGuts = UniformTime (Rotating (Toroidal (Newtonian ())))
