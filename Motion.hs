module Motion where

import Types
import Trajectory
import Grid as G

data Motion =
  Stationary GridIx Dir |
  Motion (R2,R2) [(R2,R2)] (R2,R2) GridIx Dir
    deriving Show

current :: Motion -> R2
current (Motion (x,_) _ _ _ _) = x
current (Stationary (x,y) _) = (realToFrac x, realToFrac y)

currentVel :: Motion -> R2
currentVel (Motion (_,v) _ _ _ _) = v
currentVel (Stationary _ _) = (0,0)

currentSpeed :: Motion -> R
currentSpeed = norm . currentVel

facing :: Motion -> Dir
facing (Stationary _ d) = d
facing (Motion _ _ _ _ d) = d

gridIx :: Motion -> GridIx
gridIx (Stationary ix _) = ix
gridIx (Motion _ _ _ ix _) = ix

final :: Motion -> (R2,R2)
final (Motion _ _ xv _ _) = xv

isStationary :: Motion -> Bool
isStationary (Stationary _ _) = True
isStationary _ = False

stop :: Motion -> Motion
stop (Motion xv paths xv' gix d) = Stationary gix d

--norm (x,y) = sqrt (x*x + y*y)
diff (a,b) (c,d) = (a-c,b-d)
dumb dt ((x1,x2),(v1,v2)) = ((x1 + v1*dt, x2 + v2*dt),(v1,v2))

launch :: Dir -> GridIx -> (R2,R2) -> Motion
launch dir gix xv = Motion xv [] xv gix dir

motion' :: Delta -> (R2,R2) -> ([(R2,R2)], (R2,R2))
motion' = trajectory norm diff id 5 dumb

linear :: R -> Motion -> Motion
linear _ mo@(Stationary _ _) = mo
linear dt (Motion _ _ xv ix dir) =
  let (ps, xv') = motion' dt xv in Motion xv ps xv' ix dir


timeUntilArrival :: Motion -> Maybe Double
timeUntilArrival (Motion (x,v) _ _ gix _) =
  Just $ norm (x' .-. x) / norm v where
    x' = realToFrac $$ gix
timeUntilArrival _ = Nothing

motionPlus :: R2 -> Motion -> Motion
motionPlus deltaV (Stationary ix d) = Motion (x,deltaV) [] (x,deltaV) ix d where
  x = realToFrac $$ ix
motionPlus deltaV (Motion (x,v) _ _ ix d) = Motion (x,v') [] (x,v') ix d where
  v' = v .+. deltaV

