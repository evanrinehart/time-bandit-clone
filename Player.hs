module Player where

import Types
import Animation
import Path
import Cooldown
import Joystick
import Barn
import Cyclic

-- player data structure
data Player = Player
  { plMotion  :: Motion
  , plWalking :: Cyclic Int
  , plGun     :: Cooldown
  , plFire    :: Active
  , plJoy     :: Joystick
  } deriving Show

-- default player builder
mkPlayer :: GridIx -> Player
mkPlayer gix@(i,j) = Player mo walk Ready InActive JFree where
  mo = Stationary (x,y) gix
  x = realToFrac i + 0.5
  y = realToFrac j + 0.5
  walk = mkCyclic 0.2 [0..3]

--motion' :: Delta -> (R2,R2) -> ([(R2,R2)], (R2,R2))
--linear :: R -> Motion -> Motion

-- player animation
player :: Anim Player
player dt (Player mo walk gun fire joy) = Player mo' walk' gun' fire joy where
  mo'   = linear dt mo
  walk' = cyclic (realToFrac dt) walk
  gun'  = cooldown dt gun

-- player renderer




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
