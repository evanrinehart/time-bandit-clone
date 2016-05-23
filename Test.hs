module Test where

import Data.Fixed

import Animation
import Algorithm
import Phys1
import UniformTime
import Types

type Model = UniformTime Delta (Phys1 ())
type Iface = Interface Time Model

m0 :: Model
m0 = UniformTime 0 (Phys1 (-50,-50) (0,1) ((* 10) . sombrero) ())

animation :: A Pico Model
animation = uniformTime 0.0001 (phys1 (const id))

