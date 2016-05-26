module Controller where

import Poke
import TimeBandit
import Graphics.Gloss.Interface.IO.Game

-- controller
controller :: Event -> Poke TimeBandit ()
controller e = case e of
  _ -> return ()
