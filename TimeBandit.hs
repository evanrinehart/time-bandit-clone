{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
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
import Port
import Controller

import qualified Data.IntMap as IM

-- animation
timeBandit :: Anim TimeBandit
timeBandit dt m = gameover f dt m where
  f !dt (TimeBandit' lvls pl sc hp) = TimeBandit' lvls' pl' sc hp where
    !lvls' = seq2 $ (fmap . fmap . level) dt lvls
    !pl' = playerAni dt pl

-- initial model
model0 :: Grid Tile -> TimeBandit
model0 grid = Playing $ TimeBandit' lvls player0 0 10 where
  lvls = (0, IM.fromList [(0,lvl)])
  lvl = Level 0 grid b0 b0 b0 b0 b0 []
  b0 = emptyBarn
  player0 = mkPlayer (5,7)
  
runTimeBandit :: Port FireButton
              -> Port JAction
              -> Grid Tile
              -> IO (Interface Double TimeBandit)
runTimeBandit pA pB grid = simulate (model0 grid) timeBandit
  [ playerControlRule pB
  , playerFireRule pA
  , playerUnfireRule pA
  , playerArrivalRule
  , missileExpirationRule
  ]
