{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Grid where

import Data.IntMap as IM hiding (lookup)
import qualified Data.IntMap as IM (lookup)
import Data.Maybe

data Grid a = Grid
  { gridW :: Int
  , gridH :: Int
  , gridCells :: IntMap a
  } deriving (Show, Functor)

data Cell a = Cell
  { cellHere :: a
  , cellWest :: Maybe a
  , cellNorth :: Maybe a
  , cellEast :: Maybe a
  , cellSouth :: Maybe a
  } deriving (Show, Functor)

lookup :: (Int,Int) -> Grid a -> Cell a
lookup (i,j) (Grid width h im) = Cell x w n e s where
  assert k =
    if k < 0 || k > width*h-1 then error "grid index out of bounds" else k
  !x = im ! (assert (inj i j))
  !w = IM.lookup (inj (i-1) j) im
  !n = IM.lookup (inj i (j+1)) im
  !e = IM.lookup (inj (i+1) j) im
  !s = IM.lookup (inj i (j-1)) im
  inj i j = i + width*j

fromList :: Int -> Int -> [((Int,Int),a)] -> Grid a
fromList w h kvs = g where
  g = Grid w h (IM.fromList (Prelude.map (\((i,j),x) -> (inj i j,x)) kvs))
  inj i j = i + w*j

toList :: Grid a -> [((Int,Int),a)]
toList (Grid w h im) = Prelude.map f (IM.toList im) where
  f (k,x) = ((k `mod` w, k `div` w), x)
