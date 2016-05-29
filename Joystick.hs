module Joystick where

import Data.List

data JDir = JUp | JDown | JLeft | JRight deriving (Show, Eq, Ord)
data Joystick =
  JFree |
  JPushed JDir [JDir]
    deriving Show

currentDirection :: Joystick -> Maybe JDir
currentDirection JFree = Nothing
currentDirection (JPushed d _) = Just d

lean :: JDir -> Joystick -> Joystick
lean d JFree = JPushed d []
lean d (JPushed d0 ds) = JPushed d (sort (ds `union` [d0]))

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
