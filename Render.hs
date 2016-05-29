module Render where

import Graphics.Gloss
import Types
import TimeBandit
import Level
import Grid as G
import TileGrid
import Zipper as Z
import GameOver
import Player
import Data.Monoid
import Barn as B

-- render
render :: TimeBandit -> Picture
render m = pic where
  grid = lvlGrid . Z.current . tbLvls . ungameover $ m
  gridSize = 25
  tileColor t = case t of
    Normal -> makeColor 0 0.25 0 1.0
    Wall -> black
    Door -> orange
    Teleporter _ -> magenta
    Exit -> yellow
  wholeGrid  = mconcat (map f (G.toList grid)) where
    f ((i,j), t) = color (tileColor t) $ block i j
  player (Player mo _ _ _ _) = color green (triangle 0.2 mo)
  triangle border mo = shape where
    b = border
    rot = rotate $ case B.facing mo of
      North -> 0
      West -> 270
      South -> 180
      East -> 90
    (i,j) = ((*gridSize) . realToFrac) $$ (B.current mo)
    tr = translate i j . scale gridSize gridSize . rot
    shape = tr (polygon [(0,0.5-b),(b-0.5,b-0.5),(0.5-b,b-0.5)])
  block i j =
    let (i',j') = ((*gridSize) . realToFrac) $$ (i,j) in
    let tr = translate i' j' in
    tr $ rectangleSolid gridSize gridSize
  (camX, camY) = B.current (plMotion . tbPlayer . ungameover $ m)
  globalShift = translate (gridSize * realToFrac (-camX)) (gridSize * realToFrac (-camY))
  jreport = color white $ text (show (plJoy . tbPlayer . ungameover $ m))
  pic =
    globalShift (wholeGrid <> player (tbPlayer . ungameover $ m)) <>
    translate (-300) (-200) (scale 0.2 0.2 jreport)

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
