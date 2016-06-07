module Controller where

import Prelude hiding ((.),id)
import Control.Category
import Control.Monad (when)
import Types
import Poke
import Graphics.Gloss.Interface.IO.Game as Gloss
import Joystick
import Player
import Zipper as Z
import GameOver
import Level
import Viewing
import Model
import Barn as B
import Support
import Grid
import Motion
import qualified Data.IntMap as IM
import Cooldown
import Event
import Port
import Algorithm

-- controller
controller :: Port FireButton
           -> Port JAction
           -> Iface
           -> Gloss.Event
           -> IO ()
controller pA pB iface e = let wr = simWritePort iface in case e of
  EventKey (Char 'w') Down _ _ -> wr pB (Push JUp)
  EventKey (Char 'a') Down _ _ -> wr pB (Push JLeft)
  EventKey (Char 's') Down _ _ -> wr pB (Push JDown)
  EventKey (Char 'd') Down _ _ -> wr pB (Push JRight)
  EventKey (Char 'w') Up _ _   -> wr pB (Release JUp)
  EventKey (Char 'a') Up _ _   -> wr pB (Release JLeft)
  EventKey (Char 's') Up _ _   -> wr pB (Release JDown)
  EventKey (Char 'd') Up _ _   -> wr pB (Release JRight)
  EventKey (Char 'j') Down _ _ -> wr pA FDown
  EventKey (Char 'j') Up _ _   -> wr pA FUp
  _ -> return ()

{-
playerControlRule = controlPlayer <$> whenJoystickMoves
playerFireRule = attemptFire <$> whenFireButtonIsPressed
playerUnfireRule = cancelFire <$> whenFireButtonIsReleased
-}

whenJoystickMoves :: Port JAction -> Event' JAction
whenJoystickMoves = onPort

data FireButton = FDown | FUp deriving Show

whenFireButtonIsPressed :: Port FireButton -> Event' ()
whenFireButtonIsPressed = onJust . fmap isDown . onPort where
  isDown FDown = Just ()
  isDown _ = Nothing

whenFireButtonIsReleased :: Port FireButton -> Event' ()
whenFireButtonIsReleased = onJust . fmap isUp . onPort where
  isUp FUp = Just ()
  isUp _ = Nothing

controlPlayer :: JAction -> Poke'
controlPlayer (Push jdir) = do
  grid <- viewInCurrentLevel' lvlGrid'
  update player (pressJoystick jdir)
  updateMaybe player (moveControl jdir grid)
controlPlayer (Release jdir) = update player (releaseJoystick jdir)

-- user pressed fire
-- if number of missiles is less than 2
-- then fire a missile based on player location and velocity
attemptFire :: () -> Poke'
attemptFire _ = do
  pl <- view player
  lvls <- view levels
  setFire player Active
  resetCooldown player
  when (countMissiles lvls < 2) $ do
    playerFireMissile pl lvls

cancelFire :: () -> Poke'
cancelFire _ = setFire player Inactive

playerFireMissile :: Player -> Levels -> Poke'
playerFireMissile pl (lNo,ims) = do
  let mo = plMotion pl
  let dv = 20 *. dirToVec (facing mo)
  let mo' = motionPlus dv (plMotion pl)
  newMissile lNo mo' 0
  
setFire :: PathTo Player -> Active -> Poke'
setFire player x = update (player >>> plFire') (const x)

resetCooldown :: PathTo Player -> Poke'
resetCooldown player = update (player >>> plGun') (const (Cooldown 0.5))

countMissiles :: Levels -> Int
countMissiles (_,ims) = IM.foldr ((+) . B.count . lvlMissiles) 0 ims
