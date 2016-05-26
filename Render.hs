module Render where

import Graphics.Gloss
import TimeBandit
import Level
import Grid as G
import Zipper as Z
import GameOver

-- render
render :: TimeBandit -> Picture
render m = pic where
  grid = lvlGrid . Z.current . tbLvls . ungameover $ m
  gsize = 25
  block i j c = translate (i*gsize) (j*gsize) (color c (rectangleSolid gsize gsize))
  tileColor t = case t of
    Normal -> green
    Wall -> black
    Door -> orange
    Teleporter _ -> magenta
    Exit -> yellow
  wholeGrid = mconcat (map f (G.toList grid))
  f ((i,j), t) = block (realToFrac i) (realToFrac j) (tileColor t)
  pic = wholeGrid


{-
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
-}
