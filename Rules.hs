module Rules where

import Prelude hiding ((.),id)
import Control.Category
import Types
import Poke
import Plan
import Viewing
import Algorithm
import Player
import Zipper as Z
import Level
import Model
import Support
import Data.IntMap as IM
import Barn as B

-- when a player arrives at the target square
-- then they either stop there or continue in some direction depending on
-- the surrounding tiles of the target square, the joystick direction,
-- the direction the player is facing, and a strategy
playerArrivalRule :: PathTo Player -> PathTo Levels -> Rule'
playerArrivalRule player levels = do
  (dt, joystick, cell, facing) <- whenPlayerArrives player levels
  let nextMove = nextMoveStrategy joystick cell facing
  plan dt $ case nextMove of
    StopPlayer -> stopMotion (player >>> _plMotion)
    LaunchPlayer dir -> launchPlayer player dir

-- when a missile expires, the missile is removed
missileExpirationRule :: PathTo Levels -> Rule'
missileExpirationRule levels = do
  (dt, lNo, k) <- whenMissileExpires levels
  plan dt $ do
    removeMissile levels lNo k




-- when gun is ready, fire is held, and number of missiles drops to less than 2
-- then fire another missile

