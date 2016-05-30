module Controller where

import Prelude hiding ((.),id)
import Control.Category
import Poke
import Graphics.Gloss.Interface.IO.Game
import Joystick
import Player
import Zipper as Z
import GameOver
import Level
import Viewing
import Model
import Barn

type Poke' = Poke TimeBandit ()

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
  EventKey (Char 'j') Down _ _ -> playerFire'
  EventKey (Char 'j') Up _ _   -> playerUnfire'
  _ -> return ()

playerControl' = playerControl _ppath _zlpath
playerRelease' = playerRelease _ppath
playerFire'    = playerFire _ppath _mslpath
playerUnfire'  = playerUnfire _ppath

playerControl :: PathTo Player -> PathTo (Zipper Level) -> JDir -> Poke'
playerControl ppath zlpath dir = do
  grid <- (lvlGrid . Z.current) <$> view zlpath
  update ppath (pressJoystick dir)
  updateMaybe ppath (moveControl dir grid)

playerRelease :: PathTo Player -> JDir -> Poke'
playerRelease ppath dir = do
  update ppath (releaseJoystick dir)

playerFire :: PathTo Player -> PathTo (Barn Missile) -> Poke'
playerFire player missiles = do
  return ()

playerUnfire :: PathTo Player -> Poke'
playerUnfire player = do
  return ()
