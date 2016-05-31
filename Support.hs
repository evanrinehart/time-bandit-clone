module Support where

import Prelude hiding ((.),id)
import Control.Category
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Types
import Motion as MO
import Grid as G
import TileGrid
import Joystick as J
import Player
import Poke
import Plan
import Viewing
import Algorithm
import Path
import Zipper as Z
import Level
import Model
import GameOver
import Missile
import Barn as B
import Clock

type Rule' = Rule Double TimeBandit
type Poke' = Poke TimeBandit ()
type Plan' a = Plan TimeBandit a


whenPlayerArrives :: PathTo Player
                  -> PathTo Levels
                  -> Plan' (Double, Maybe JDir, Cell Tile, Dir)
whenPlayerArrives _player _levels = do
  dt <- required =<< timeUntilArrival <$> view (_player >>> _plMotion)
  pl <- view _player
  grid <- viewInCurrentLevel _levels _lvlGrid
  let mo = plMotion pl
  let gix = gridIx mo
  let Just cell = G.lookup gix grid
  return (dt, J.currentDirection (plJoy pl), cell, facing mo)

stopMotion :: PathTo Motion -> Poke'
stopMotion _mo = update _mo MO.stop

launchPlayer :: PathTo Player -> Dir -> Poke'
launchPlayer _player dir = do
  let _mo = _player >>> _plMotion
  mo <- view _mo
  let gix = gridIx mo
  let gix' = gixPlusDir dir gix
  let x' = realToFrac $$ gix
  let v' = MO.currentSpeed mo *. dirToVec dir
  update _mo (const (launch dir gix' (x',v')))

--viewInCurrentLevel :: PathTo Levels -> Path Level a -> Plan' a
viewInCurrentLevel _levels _p = do
  k <- view (_levels >>> _first)
  view (_levels >>> _second >>> _imkey k >>> _p)

newMissile :: LevelNo -> Motion -> MissileType -> Poke'
newMissile k mo mty =
  update
  (_playing >>> _tbLvls >>> _second >>> _imkey k >>> _lvlMissiles)
  (insertThing mo (mkMissile 0))

removeMissile :: PathTo Levels -> LevelNo -> Int -> Poke'
removeMissile levels lNo k =
  update
  (levels >>> _second >>> _imkey lNo >>> _lvlMissiles)
  (B.deleteThing k)

whenMissileExpires :: PathTo Levels -> Plan' (Double, LevelNo, Int)
whenMissileExpires levels = ans where
  ans = do
    (_,im) <- view levels
    required $ IM.foldrWithKey f Nothing im
  f lNo lvl Nothing = B.foldr (g lNo) Nothing (lvlMissiles lvl)
  f lNo lvl accum = B.foldr (g lNo) accum (lvlMissiles lvl)
  g lNo k (Missile (Clock dt _) _) Nothing = Just (dt, lNo, k)
  g lNo k (Missile (Clock dt _) _) r@(Just (dt0, lNo0, k0)) = if dt < dt0
    then Just (dt, lNo, k)
    else r

