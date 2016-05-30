{-# LANGUAGE MultiWayIf #-}
module Player where

import Types
import Animation
import Path
import Cooldown
import Joystick as J
import Barn as B
import Cyclic
import Grid as G
import TileGrid
import Motion as MO

-- player data structure
data Player = Player
  { plMotion  :: Motion
  , plWalking :: Cyclic Int
  , plGun     :: Cooldown
  , plFire    :: Active
  , plJoy     :: Joystick
  } deriving Show

_plMotion :: Path Player Motion
_plMotion = Path (w8 0) (Just . plMotion) s where
  s f (Player u v w x y) = Player (f u) v w x y

-- default player builder
mkPlayer :: GridIx -> Player
mkPlayer gix@(i,j) = Player mo walk Ready InActive JFree where
  mo = Stationary gix South
  walk = mkCyclic 0.2 [0..3]

--motion' :: Delta -> (R2,R2) -> ([(R2,R2)], (R2,R2))
--linear :: R -> Motion -> Motion

-- player animation
player :: Anim Player
player dt (Player mo walk gun fire joy) = p where
  p = Player mo' walk' gun' fire joy
  mo'   = linear dt mo
  walk' = cyclic (realToFrac dt) walk
  gun'  = cooldown dt gun

onJS :: (Joystick -> Joystick) -> Player -> Player
onJS f pl = pl { plJoy = (f (plJoy pl)) }

pressJoystick :: JDir -> Player -> Player
pressJoystick dir = onJS (J.lean dir)

releaseJoystick :: JDir -> Player -> Player
releaseJoystick dir = onJS (J.release dir)

-- if the player is stationary, the direction pressed agrees
-- with the joystick total direction, and the way isnt blocked
-- then launch the player in that direction
moveControl :: JDir -> Grid Tile -> Player -> Maybe Player
moveControl jdir grid pl =
  let mo = plMotion pl in
  let gix = gridIx mo in
  let Just cell = G.lookup gix grid in
  let fdir = facing mo in
  case nextMoveStrategy (Just jdir) cell fdir of
    StopPlayer -> Nothing
    LaunchPlayer dir' ->
      if isStationary mo then Just $ pl { plMotion = mo' } else Nothing where
        mo' = launch dir' gix' (x,v')
        x = MO.current mo
        gix' = gixPlusDir dir' gix
        speed = 8
        s = speed
        v' = speed *. dirToVec dir'

data ArrivalResult = StopPlayer | LaunchPlayer Dir deriving Show

nextMoveStrategy :: Maybe JDir -> Cell Tile -> Dir -> ArrivalResult
nextMoveStrategy mjdir cell facingDir = case mjdir of
  Nothing -> StopPlayer
  Just jdir ->
    let jdir' = jsdirToDir jdir in
    let rightSide = rightHandSide facingDir in
    let leftSide = leftHandSide facingDir in
    let dirOpen d = not (barrierInDirection d cell) in
    if | dirOpen jdir'      -> LaunchPlayer jdir'
       | dirOpen facingDir  -> LaunchPlayer facingDir
       | jdir' /= facingDir -> StopPlayer
       | dirOpen leftSide   -> LaunchPlayer leftSide
       | dirOpen rightSide  -> LaunchPlayer rightSide
       | otherwise          -> StopPlayer

cellInDirection :: JDir -> Grid a -> GridIx -> Maybe (Cell a)
cellInDirection dir grid gix = G.lookup (gixPlusDir (jsdirToDir dir) gix) grid

isWall :: Cell Tile -> Bool
isWall (Cell Wall _ _ _ _) = True
isWall _ = False

isDoor :: Cell Tile -> Bool
isDoor (Cell Door _ _ _ _) = True
isDoor _ = False


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

-- player rules
-- - hold fire while standing
-- - hold fire while running
-- - press joystick while running
-- - release joystick while running
-- - press joystick while standing
-- - arrive at square
-- - press joystick while standing and holding fire
-- - collide with treasure
-- - collide with monster
-- - collide with enemy missile








