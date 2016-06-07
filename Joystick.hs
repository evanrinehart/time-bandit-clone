module Joystick where

import Data.List

data JDir = JUp | JDown | JLeft | JRight deriving (Show, Eq, Ord)

data Joystick =
  JFree |
  JPushed JDir [JDir]
    deriving Show

data JAction = Push JDir | Release JDir deriving (Show, Eq, Ord)

currentDirection :: Joystick -> Maybe JDir
currentDirection JFree = Nothing
currentDirection (JPushed d _) = Just d

push :: JDir -> Joystick -> Joystick
push d JFree = JPushed d []
push d (JPushed d0 ds) = JPushed d (sort (ds `union` [d0]))

release :: JDir -> Joystick -> Joystick
release _ JFree = JFree
release x j@(JPushed y [])
  | x == y = JFree
  | otherwise = j
release x (JPushed y zs)
  | x == y = case zs \\ [x] of
      [] -> JFree
      (z:zs') -> JPushed z zs'
  | otherwise = JPushed y (zs \\ [x])
