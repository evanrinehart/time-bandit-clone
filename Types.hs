module Types where

import Animation

type Anim a = A Double a

type R = Double
type R2 = (R,R)
type Delta = Double
type Time = Delta

data Active = Active | InActive deriving Show

getAngle :: R2 -> R2 -> R
getAngle u v | norm u == 0 || norm v == 0 = 0
             | otherwise =
  let (x1,x2) = unitv u in
  let (y1,y2) = unitv v in
  let alpha = acos (x1*y1 + x2*y2) in
  let beta  = asin (x1*y2 - x2*y1) in
  if signum beta == 0 then alpha else alpha * signum beta

(a,b) .-. (c,d) = (a-c, b-d)
r *. (a,b) = (r*a, r*b)
f $$ (x,y) = (f x, f y)

dot :: R2 -> R2 -> R
dot (a,b) (c,d) = a*c + b*d

unitv :: R2 -> R2
unitv (x,y) = (x / norm (x,y), y / norm (x,y)) 

norm (x,y) = sqrt (x*x + y*y)

radToDeg rad = 180*rad/pi
