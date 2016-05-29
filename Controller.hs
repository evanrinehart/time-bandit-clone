module Controller where

import Prelude hiding ((.),id)
import Control.Category
import Poke
import TimeBandit
import Graphics.Gloss.Interface.IO.Game
import Joystick
import Player
import Zipper as Z
import GameOver
import Level
import Viewing

-- controller
controller :: Event -> Poke TimeBandit ()
controller e = case e of
  EventKey (Char 'w') Down _ _ -> playerControl' JUp
  EventKey (Char 'a') Down _ _ -> playerControl' JLeft
  EventKey (Char 's') Down _ _ -> playerControl' JDown
  EventKey (Char 'd') Down _ _ -> playerControl' JRight
  EventKey (Char 'w') Up _ _   -> playerRelease' JUp
  EventKey (Char 'a') Up _ _   -> playerRelease' JLeft
  EventKey (Char 's') Up _ _   -> playerRelease' JDown
  EventKey (Char 'd') Up _ _   -> playerRelease' JRight
  _ -> return ()

playerControl' = playerControl ppath zlpath
playerRelease' = playerRelease ppath

playerControl :: PathTo Player
              -> PathTo (Zipper Level)
              -> JDir
              -> Poke TimeBandit ()
playerControl ppath zlpath dir = do
  grid <- (lvlGrid . Z.current) <$> view zlpath
  update ppath (pressJoystick dir)
  updateMaybe ppath (moveControl dir grid)

playerRelease ppath dir = do
  update ppath (releaseJoystick dir)
