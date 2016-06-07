module Model where

import Prelude hiding ((.),id)
import Control.Category
import Data.IntMap.Strict (IntMap)
import Path
import GameOver
import Zipper
import Level
import Player
import Algorithm

type Iface = Interface Double TimeBandit
type PathTo = Path TimeBandit
type TimeBandit = GameOver TimeBandit'
data TimeBandit' = TimeBandit'
  { tbLvls :: !Levels
  , tbPlayer :: !Player
  , tbScore :: !Int
  , tbHP :: !Int
  } deriving Show

tbLvls' :: Path TimeBandit' Levels
tbLvls' = Path (w8 0) g s where
  g = Just . tbLvls
  s f tb = tb { tbLvls = f (tbLvls tb) }

tbPlayer' :: Path TimeBandit' Player
tbPlayer' = Path (w8 1) g s where
  g = Just . tbPlayer
  s f tb = tb { tbPlayer = f (tbPlayer tb) }

