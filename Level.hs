{-# LANGUAGE DeriveFunctor #-}
module Level where

import Grid as G
import Barn
import Types
import Zipper
import Player

data Tile =
  Normal |
  Wall |
  Door |
  Teleporter (Int,(Int,Int)) |
  Exit
    deriving Show

data Level = Level
  { lvlNo :: Int
  , lvlGrid :: Grid Tile
  , lvlBarn1 :: Barn Player
  , lvlBarn2 :: Barn Monster
  , lvlBarn3 :: Barn Missile
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


rawData :: [[Int]]
rawData =
  [[1,1,1,1,1,1,1,1,1,1,1,1]
  ,[1,0,0,1,0,0,0,0,0,0,0,1]
  ,[1,0,0,0,0,0,0,0,0,1,1,1]
  ,[1,0,0,1,0,0,0,0,0,0,0,1]
  ,[1,0,0,1,0,0,0,0,0,1,0,1]
  ,[1,1,1,1,1,0,1,1,1,1,1,1]
  ,[1,0,0,0,1,0,1,0,0,0,0,1]
  ,[1,0,4,0,2,0,1,0,1,0,1,1]
  ,[1,0,0,0,1,0,0,0,1,0,0,1]
  ,[1,1,1,1,1,1,1,1,1,1,1,1]]

readRaw :: [[Int]] -> Grid Tile
readRaw [] = G.fromList 0 0 []
readRaw rows = answer where
  height = length rows
  width = length (head rows)
  cells = concat $ zipWith f (reverse [0..height-1]) rows
  f j row = zipWith (\i n -> ((i,j), g n)) [0..] row
  g 0 = Normal
  g 1 = Wall
  g 2 = Door
  g 3 = Teleporter (0,(0,0))
  g 4 = Exit
  answer = G.fromList width height cells

exampleGrid = readRaw rawData
