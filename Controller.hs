module Controller where

import Prelude hiding ((.),id)
import Control.Category
import Control.Monad (when)
import Types
import Poke
import Graphics.Gloss.Interface.IO.Game
import Joystick
import Player
import Zipper as Z
import GameOver
import Level
import Viewing
import Model
import Barn as B
import Support
import Grid
import Motion
import qualified Data.IntMap as IM
import Cooldown

-- controller
controller :: Event -> Poke TimeBandit ()
controller e = case e of
  EventKey (Char 'w') Down _ _ -> playerControl' JUp
  EventKey (Char 'a') Down _ _ -> playerControl' JLeft
  EventKey (Char 's') Down _ _ -> playerControl' JDown
  EventKey (Char 'd') Down _ _ -> playerControl' JRight
  EventKey (Char 'w') Up _ _   -> playerRelease' JUp
  EventKey (Char 'a') Up _ _   -> playerRelease' JLeft
  EventKey (Char 's') Up _ _   -> playerRelease' JDown
  EventKey (Char 'd') Up _ _   -> playerRelease' JRight
  EventKey (Char 'j') Down _ _ -> playerFire'
  EventKey (Char 'j') Up _ _   -> playerUnfire'
  _ -> return ()

playerControl' = playerControl _ppath _lvlspath
playerRelease' = playerRelease _ppath
playerFire'    = playerFire _ppath _lvlspath
playerUnfire'  = playerUnfire _ppath

playerControl :: PathTo Player -> PathTo Levels -> JDir -> Poke'
playerControl player levels dir = do
  grid <- viewInCurrentLevel levels _lvlGrid
  update player (pressJoystick dir)
  updateMaybe player (moveControl dir grid)

playerRelease :: PathTo Player -> JDir -> Poke'
playerRelease player dir = do
  update player (releaseJoystick dir)

-- user pressed fire
-- if number of missiles is less than 2
-- then fire a missile based on player location and velocity
playerFire :: PathTo Player -> PathTo Levels -> Poke'
playerFire player levels = do
  pl <- view player
  lvls <- view levels
  setFire player Active
  resetCooldown player
  when (countMissiles lvls < 10) $ do
    playerFireMissile pl lvls

playerUnfire :: PathTo Player -> Poke'
playerUnfire player = setFire player Inactive

playerFireMissile :: Player -> Levels -> Poke'
playerFireMissile pl (lNo,ims) = do
  let mo = plMotion pl
  let dv = 20 *. dirToVec (facing mo)
  let mo' = motionPlus dv (plMotion pl)
  newMissile lNo mo' 0
  
setFire :: PathTo Player -> Active -> Poke'
setFire player x = update (player >>> _plFire) (const x)

resetCooldown :: PathTo Player -> Poke'
resetCooldown player = update (player >>> _plGun) (const (Cooldown 0.5))

countMissiles :: Levels -> Int
countMissiles (_,ims) = IM.foldr ((+) . B.count . lvlMissiles) 0 ims
