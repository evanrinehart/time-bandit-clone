{-# LANGUAGE DeriveFunctor #-}
module Level where

import Grid as G
import TileGrid
import Barn
import Types
import Zipper
import Player
import Path

data Level = Level
  { lvlNo :: Int
  , lvlGrid :: Grid Tile
  , lvlOtherPlayers :: Barn Player
  , lvlBarn2 :: Barn Monster
  , lvlMissiles :: Barn Missile
  , lvlBarn4 :: Barn Treasure
  , lvlBarn5 :: Barn Smoke
  , lvlGens :: [((Int, Int), GenMonster)]
  } deriving Show
  
type Vel = R2
data Monster = Monster deriving Show
data Missile = Missile deriving Show
data Treasure = Treasure deriving Show
data Smoke = Smoke deriving Show

newtype GenMonster = GenMonster { genMonster :: Vel -> Monster }
instance Show GenMonster where
  show _ = "<Vel -> Monster>"

level :: Anim Level
level dt (Level i g b1 b2 b3 b4 b5 gens) = updatedLevel where
  updatedLevel = Level i g b1' b2' b3' b4' b5' gens
  b1' = barn (const id) dt b1
  b2' = barn (const id) dt b2
  b3' = barn (const id) dt b3
  b4' = barn (const id) dt b4
  b5' = barn (const id) dt b5

_missiles :: Path Level (Barn Missile)
_missiles = Path (w8 3) (Just . lvlMissiles) s where
  s f l = l { lvlMissiles = (f (lvlMissiles l)) }
