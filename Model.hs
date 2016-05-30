module Model where

import Prelude hiding ((.),id)
import Control.Category
import Path
import GameOver
import Zipper
import Level
import Player

type PathTo = Path TimeBandit
type TimeBandit = GameOver TimeBandit'
data TimeBandit' = TimeBandit'
  { tbLvls :: Zipper Level
  , tbPlayer :: Player
  , tbScore :: Int
  , tbHP :: Int
  } deriving Show

_ppath = _playing >>> _player
_zlpath = _playing >>> _levels
_mslpath = _playing >>> _levels >>> _current >>> _missiles

_levels :: Path TimeBandit' (Zipper Level)
_levels = Path (w8 0) g s where
  g = Just . tbLvls
  s f tb = tb { tbLvls = f (tbLvls tb) }

_player :: Path TimeBandit' Player
_player = Path (w8 1) g s where
  g = Just . tbPlayer
  s f tb = tb { tbPlayer = f (tbPlayer tb) }

