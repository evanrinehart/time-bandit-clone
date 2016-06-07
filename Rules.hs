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
import Controller

playerControlRule p = controlPlayer <$> whenJoystickMoves p
playerFireRule p = attemptFire <$> whenFireButtonIsPressed p
playerUnfireRule p = cancelFire <$> whenFireButtonIsReleased p
playerArrivalRule = redirectPlayer <$> whenPlayerArrives
missileExpirationRule = removeMissile <$> whenMissileExpires


-- when gun is ready, fire is held, and number of missiles drops to less than 2
-- then fire another missile

