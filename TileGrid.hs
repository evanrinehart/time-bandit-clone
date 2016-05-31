module TileGrid where

import Data.Maybe

import Types
import Grid as G
import Joystick as J

data Tile =
  Normal |
  Wall |
  Door |
  Teleporter (Int,(Int,Int)) |
  Exit
    deriving Show

barrierInDirection :: Dir -> Cell Tile -> Bool
barrierInDirection d = maybe True isBarrier . cellDir d

isBarrier :: Tile -> Bool
isBarrier Wall = True
isBarrier Door = True
isBarrier _ = False

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

isWall :: Cell Tile -> Bool
isWall (Cell Wall _ _ _ _) = True
isWall _ = False

isDoor :: Cell Tile -> Bool
isDoor (Cell Door _ _ _ _) = True
isDoor _ = False

cellInDirection :: JDir -> Grid a -> GridIx -> Maybe (Cell a)
cellInDirection dir grid gix = G.lookup (gixPlusDir (jsdirToDir dir) gix) grid

jsdirToDir d = case d of
  JUp    -> North
  JRight -> East
  JDown  -> South
  JLeft  -> West

rightHandSide :: Dir -> Dir
rightHandSide d = case d of
  North -> East
  East  -> South
  South -> West
  West  -> North

leftHandSide :: Dir -> Dir
leftHandSide d = case d of
  North -> West
  West  -> South
  South -> East
  East  -> North

