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
import Air
import Clock

screenW = 640
screenH = 480

main = do
  let mode = (InWindow "Testing" (screenW,screenH) (0,0))
  iface <- run mySim0
  playIO mode black 60 iface render input wait

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

wait :: Float -> Iface -> IO Iface
wait dt iface = do
  (iface & simWait) (realToFrac dt)
  im <- simImage iface id
  inner <- simDebug iface
  --print inner
  return iface

render :: Iface -> IO Picture
render iface = simImage iface f where
  f s = planet <> (tr ship) <> timer <> aDot where
    planet = color green $ circle 25
    tr =
      translate (realToFrac x) (realToFrac y) .
      rotate (realToFrac angle) .
      color cyan
    ship = scale 10 10 $ lineLoop [(0,1),(0.5,-1),(0,-0.5),(-0.5,-1)]
    (x,y) = (physX . uniformBody . snd) s
    (vx,vy) = (physV . uniformBody . snd) s
    timerTr = translate (-300) (200) . scale 0.1 0.1 . color white
    timer = timerTr $ case (snd . fst) s of
      HaveAir -> blank
      AirRunningOut cl -> text (show (clockT cl))
    angle = radToDeg $ getAngle (vx,vy) (0,1)
    aDot = tr (circleSolid 1.5)
