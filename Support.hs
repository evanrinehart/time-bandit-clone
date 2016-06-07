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
import Event
import Prediction

type Rule' = Rule Double TimeBandit
type Poke' = Poke TimeBandit ()
type Plan' a = Plan TimeBandit a
type Event' a = Event Double TimeBandit a

core = playing'
player = core >>> tbPlayer'
playerMotion = player >>> plMotion'
levels = core >>> tbLvls' :: Path TimeBandit Levels
missilesOnLevel k = levels >>> right >>> imkey k >>> lvlMissiles'

whenPlayerArrives :: Event' (Maybe JDir, Cell Tile, Dir)
whenPlayerArrives = predict $ do
  dt <- required =<< timeUntilArrival <$> view playerMotion
  pl <- view player
  grid <- viewInCurrentLevel lvlGrid'
  let mo = plMotion pl
  let gix = gridIx mo
  let Just cell = G.lookup gix grid
  return (InExactly dt (J.currentDirection (plJoy pl), cell, facing mo))

redirectPlayer :: (Maybe JDir, Cell Tile, Dir) -> Poke'
redirectPlayer (joystick, cell, facing) = do
  case nextMoveStrategy joystick cell facing of
    StopPlayer -> stopMotion playerMotion
    LaunchPlayer dir -> launchPlayer player dir

stopMotion :: PathTo Motion -> Poke'
stopMotion mo = update mo MO.stop

launchPlayer :: PathTo Player -> Dir -> Poke'
launchPlayer _player dir = do
  mo <- view playerMotion
  let gix = gridIx mo
  let gix' = gixPlusDir dir gix
  let x' = realToFrac $$ gix
  let v' = MO.currentSpeed mo *. dirToVec dir
  update playerMotion (const (launch dir gix' (x',v')))

viewInCurrentLevel :: Path Level a -> Plan TimeBandit a
viewInCurrentLevel p = do
  k <- view (levels >>> left)
  view (levels >>> right >>> imkey k >>> p)

viewInCurrentLevel' :: Path Level a -> Poke TimeBandit a
viewInCurrentLevel' p = do
  k <- view (levels >>> left)
  view (levels >>> right >>> imkey k >>> p)

newMissile :: LevelNo -> Motion -> MissileType -> Poke'
newMissile k mo mty =
  update (missilesOnLevel k) (insertThing mo (mkMissile 0))

removeMissile :: (LevelNo, Int) -> Poke'
removeMissile (lNo, mNo) =
  update (missilesOnLevel lNo) (B.deleteThing mNo)

whenMissileExpires :: Event' (LevelNo, Int)
whenMissileExpires = predict ans where
  f lNo lvl Nothing = B.foldr (g lNo) Nothing (lvlMissiles lvl)
  f lNo lvl accum = B.foldr (g lNo) accum (lvlMissiles lvl)
  g lNo k (Missile (Clock dt _) _) Nothing = Just (dt, lNo, k)
  g lNo k (Missile (Clock dt _) _) r@(Just (dt0, lNo0, k0)) = if dt < dt0
    then Just (dt, lNo, k)
    else r
  ans = do
    (_,im) <- view levels
    (dt,lno,missile) <- required $ IM.foldrWithKey f Nothing im
    return (InExactly dt (lno,missile))
