module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Vector as V
import Data.Monoid
import System.Exit

import Data.Fixed

import Animation
import Algorithm
import Phys1
import Test
import UniformTime

screenW = 640
screenH = 480

main = do
  let mode = (InWindow "Testing" (screenW,screenH) (0,0))
  let sim = newSimulation m0 animation [] []
  iface <- run sim
  playIO mode black 20 iface render input time

input :: Event -> Iface -> IO Iface
input e iface = case e of
  EventKey _ _ _ _ -> do
    print e
    iface & simKill
    exitSuccess
  _ -> do
    print e
    return iface

x & f = f x

time :: Float -> Iface -> IO Iface
time dt iface = do
  print dt
  (iface & simWait) (realToFrac dt)
  im <- simImage iface id
  print im
  return iface

render :: Iface -> IO Picture
render iface = do
  (x,y) <- simImage iface (physX . uniformBody)
  let tr = translate (realToFrac x) (realToFrac y) . color blue
  let sh = rectangleSolid 100 100
  return (tr sh)
