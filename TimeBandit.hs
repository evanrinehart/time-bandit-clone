{-# LANGUAGE GADTs #-}
module TimeBandit where

import Prelude hiding ((.),id)
import Control.Category
import Types
import Animation
import Level
import Barn
import Poke
import Grid as G
import TileGrid
import GameOver
import Algorithm
import Zipper as Z
import Player
import Path
import Joystick as J
import Plan
import Viewing

type PathTo = Path TimeBandit
type TimeBandit = GameOver TimeBandit'
data TimeBandit' where
  TimeBandit' ::
    { tbLvls :: Zipper Level
    , tbPlayer :: Player
    , tbScore :: Int
    , tbHP :: Int } -> TimeBandit'
      deriving Show

_levels :: Path TimeBandit' (Zipper Level)
_levels = Path (w8 0) g s where
  g = Just . tbLvls
  s f tb = tb { tbLvls = f (tbLvls tb) }

_player :: Path TimeBandit' Player
_player = Path (w8 1) g s where
  g = Just . tbPlayer
  s f tb = tb { tbPlayer = f (tbPlayer tb) }

-- animation
timeBandit :: Anim TimeBandit
timeBandit dt m = gameover f dt m where
  f dt (TimeBandit' lvls pl sc hp) = TimeBandit' lvls' pl' sc hp where
    lvls' = (fmap . level) dt lvls
    pl' = player dt pl

-- initial model
model0 :: TimeBandit
model0 = Playing $ TimeBandit' lvls player0 0 10 where
  lvls = singleton lvl
  lvl = Level 0 mygrid b0 b0 b0 b0 b0 []
  mygrid = exampleGrid
  b0 = emptyBarn
  player0 = mkPlayer (5,7)

-- when a player arrives at the target square
-- and the joystick is free, then he stops there
-- otherwise he continues in the direction of the joystick (if possible)
playerArrivalRule :: PathTo Player
                  -> PathTo (Zipper Level)
                  -> Rule Double TimeBandit 
playerArrivalRule _player zlpath = Rule "playerArrival" $ do
  dt <- required =<< timeUntilArrival <$> view (_plMotion . _player)
  pl <- view _player
  let foo = plMotion pl
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

timeUntilArrival :: Motion -> Maybe Double
timeUntilArrival (Motion (x,v) _ _ gix _) =
  Just $ norm (x' .-. x) / norm v where
    x' = realToFrac $$ gix
timeUntilArrival _ = Nothing
  
ppath = playing >>> _player
zlpath = playing >>> _levels
simulation = newSimulation timeBandit model0
  [playerArrivalRule ppath zlpath]
  []


-- model
-- animation
-- rendering
-- controller
-- rules

-- goal... player object



