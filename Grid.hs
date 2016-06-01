{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Grid where

import Types
import Data.IntMap.Strict as IM hiding (lookup)
import qualified Data.IntMap.Strict as IM (lookup)
import Data.Maybe

data Grid a = Grid
  { gridW :: !Int
  , gridH :: !Int
  , gridCells :: IntMap a
  } deriving (Show, Functor)

data Cell a = Cell
  { cellHere  :: a
  , cellNorth :: Maybe a
  , cellEast  :: Maybe a
  , cellSouth :: Maybe a
  , cellWest  :: Maybe a
  } deriving (Show, Functor)

type GridIx = (Int,Int)
data Dir = North | South | East | West deriving (Eq, Show)

lookup :: (Int,Int) -> Grid a -> Maybe (Cell a)
lookup (i,j) (Grid width h im) =
  let inj i j = i + width*j in
  case IM.lookup (inj i j) im of
    Nothing -> Nothing
    Just !x -> Just (Cell x n e s w) where
      !n = IM.lookup (inj i (j+1)) im
      !e = IM.lookup (inj (i+1) j) im
      !s = IM.lookup (inj i (j-1)) im
      !w = IM.lookup (inj (i-1) j) im

fromList :: Int -> Int -> [((Int,Int),a)] -> Grid a
fromList w h kvs = g where
  g = Grid w h (IM.fromList (Prelude.map (\((i,j),x) -> (inj i j,x)) kvs))
  inj i j = i + w*j

toList :: Grid a -> [((Int,Int),a)]
toList (Grid w h im) = Prelude.map f (IM.toList im) where
  f (k,x) = ((k `mod` w, k `div` w), x)

cellDir :: Dir -> Cell a -> Maybe a
cellDir North = cellNorth
cellDir East  = cellEast
cellDir South = cellSouth
cellDir West  = cellWest

gixPlusDir :: Dir -> GridIx -> GridIx
gixPlusDir d (i,j) = case d of
  North -> (i,j+1)
  East  -> (i+1,j)
  South -> (i,j-1)
  West  -> (i-1,j)

dirToVec :: Dir -> R2
dirToVec North = ( 0, 1)
dirToVec East  = ( 1, 0)
dirToVec South = ( 0,-1)
dirToVec West  = (-1, 0)
