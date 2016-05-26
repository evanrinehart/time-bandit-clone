{-# LANGUAGE GADTs #-}
module TimeBandit where

import Types
import Animation
import Level
import Barn
import Poke
import Grid as G
import GameOver
import Algorithm
import Zipper as Z
import Player

type TimeBandit = GameOver TimeBandit'
data TimeBandit' where
  TimeBandit' ::
    { tbLvls :: Zipper Level
    , tbPlayer :: Player
    , tbScore :: Int
    , tbHP :: Int } -> TimeBandit'
      deriving Show

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


simulation = newSimulation timeBandit model0
  []
  []
