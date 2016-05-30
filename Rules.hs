module Rules where

import Prelude hiding ((.),id)
import Control.Category
import Types
import Motion
import Grid as G
import Joystick as J
import Player
import Poke
import Plan
import Viewing
import Algorithm
import Zipper as Z
import Level
import Model
import GameOver

type Rule' = Rule Double TimeBandit

-- when a player arrives at the target square
-- then they either stop or continue in some direction according to
-- the surrounding tiles of the target square, the joystick direction,
-- the direction the player is facing, and a strategy
playerArrivalRule :: PathTo Player -> PathTo (Zipper Level) -> Rule'
playerArrivalRule _player zlpath = Rule "playerArrival" $ do
  dt <- required =<< timeUntilArrival <$> view (_plMotion . _player)
  pl <- view _player
  grid <- lvlGrid . Z.current <$> view zlpath
  let mo = plMotion pl
  let facedir = facing mo
  let mjdir = J.currentDirection (plJoy pl)
  let gix = gridIx mo
  let Just cell = G.lookup gix grid
  plan dt $ case nextMoveStrategy mjdir cell facedir of
    StopPlayer -> update (_player >>> _plMotion) stopMotion
    LaunchPlayer dir' -> update (_player >>> _plMotion) f where
      f (Motion (_,v) _ _ _ _) = launch dir' gix' (x',v') where
        speed = norm v
        v' = speed *. dirToVec dir'
        x' = realToFrac $$ gix
        gix' = gixPlusDir dir' gix
