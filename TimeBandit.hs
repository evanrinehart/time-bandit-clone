{-# LANGUAGE GADTs #-}
module TimeBandit where

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
import Rules
import Model

import qualified Data.IntMap as IM

-- animation
timeBandit :: Anim TimeBandit
timeBandit dt m = gameover f dt m where
  f dt (TimeBandit' lvls pl sc hp) = TimeBandit' lvls' pl' sc hp where
    lvls' = (fmap . fmap . level) dt lvls
    pl' = player dt pl

-- initial model
model0 :: TimeBandit
model0 = Playing $ TimeBandit' lvls player0 0 10 where
  lvls = (0, IM.fromList [(0,lvl)])
  lvl = Level 0 mygrid b0 b0 b0 b0 b0 []
  mygrid = exampleGrid
  b0 = emptyBarn
  player0 = mkPlayer (5,7)
  
runTimeBandit :: IO (Interface Double TimeBandit)
runTimeBandit = runSim timeBandit model0
  [ playerArrivalRule _ppath _lvlspath
  , missileExpirationRule _lvlspath
  ]
