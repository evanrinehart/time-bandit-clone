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
import Types

screenW = 640
screenH = 480

main = do
  let mode = (InWindow "Testing" (screenW,screenH) (0,0))
  let sim = newSimulation m0 animation [] []
  iface <- run sim
  playIO mode black 60 iface render input time

input :: Event -> Iface -> IO Iface
input e iface = foo >> return iface where
  foo = case e of
    EventKey (Char 'w') dir _ _ -> simPoke iface (control (0,100) (dir==Down))
    EventKey (Char 'a') dir _ _ -> simPoke iface (control (-100,0) (dir==Down))
    EventKey (Char 's') dir _ _ -> simPoke iface (control (0,-100) (dir==Down))
    EventKey (Char 'd') dir _ _ -> simPoke iface (control (100,0) (dir==Down))
    EventKey (SpecialKey KeyEsc) _ _ _ -> do
      iface & simKill
      exitSuccess
    _ -> return ()

x & f = f x

time :: Float -> Iface -> IO Iface
time dt iface = do
  (iface & simWait) (realToFrac dt)
  im <- simImage iface id
  return iface

render :: Iface -> IO Picture
render iface = do
  (x,y) <- simImage iface (physX . uniformBody)
  (vx,vy) <- simImage iface (physV . uniformBody)
  let angle = radToDeg $ getAngle (vx,vy) (0,1)
  let planet = color green $ circle 10
  let tr =
        translate (realToFrac x) (realToFrac y) .
        rotate (realToFrac angle) .
        color cyan
  let ship = scale 10 10 $ lineLoop [(0,1),(0.5,-1),(0,-0.5),(-0.5,-1)]
  return (planet <> (tr ship))
