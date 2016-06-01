module LinePoint where

-- solve the following problem:
--
-- a point is moving through space with uniform linear motion
--   x = x0 + vx*t where t in [t0, t1]
-- a line segment moves such that its end points are moving as above
--   a = a0 + va*t where t in [t0, t1]
--   b = b0 + vb*t where t in [t0, t1]
--   seg = a + (b - a)*s where s in [0,1]
--     where x x0 vx a x0 va b b0 and vb are vectors
--     and seg is a line segment with endpoints a,b and parameter s
--
-- solve for the 0 1 or 2 times in [t0,t1] that the point crosses the line.
-- if the point is on an overlapping parallel path with the line, no solution.

-- notes:
-- basically the problem is to solve for t where x = seg and both t and s
-- are within the specified range. plugging the definitions into the equation:
-- x0 + vx*t = a0 + va*t + (b0 + vb*t - a0 - va*t)*s
-- which rearranges to
-- 0 = (a0 - x0) + (va - vx)*t + (b0 - a0)*s + (vb - va)*s*t
-- which is a bilinear equation. If the coefficients were scalar constants
-- then the solution set of (s,t) pairs where the RHS equals some constant
-- would be a pair of hyperbolas in the s-t plane. there are always infinitely
-- many solutions. But because they are 2D vectors, this is actually two
-- equations, one for each dimension.
--
-- let A = a0 - x0
-- let B = va - vx
-- let C = b0 - a0
-- let D = vb - va
-- and (A1,A2) be the two components of A and so on
--
-- the two equations to solve are now:
-- 0 = A1 + B1*t + C1*s + D1*s*t
-- 0 = A2 + B2*t + C2*s + D2*s*t
--
-- solving the second equation for s gives
-- s = -(A2 + B2*t)/(C2 + D2*t)
--   (generating a new assumption that C2+D2*t != 0 for the interesting t)
-- pluggin that into the first equation gives
-- (A2 + B2*t)/(C2 + D2*t) = (A1 + B1*t)/(C1 + D1*t)
--   (generating a new assumption that C1 + D1*t != 0 for interesting t)
--
-- rearranging that gives
-- 0 = (A2*C1 - A1*C2) + (B2*D1 - B1*D2 + A2*D1 - A1*D2)*t + (B2*D1 - B1*D2)*t^2
--
-- which is a quadratic equation with 0 1 or 2 solutions. all that is left
-- to do is the make sure all solutions are between t0 and t1 and that the
-- implied s is between 0 and 1.

import Types

--type R = Double
--type R2 = (R,R)

data Problem = Problem
  { _x0 :: R2
  , _vx :: R2
  , _a0 :: R2
  , _va :: R2
  , _b0 :: R2
  , _vb :: R2
  , _t0 :: R
  , _t1 :: R } deriving Show

data Solution = NoSolution | OneSolution !R | TwoSolutions !R !R deriving Show

solve :: Problem -> Solution
solve (Problem x0 vx a0 va b0 vb t0 t1) = filterSol redact answer where
  p1 = fst a0 - fst x0
  q1 = fst va - fst vx
  u1 = fst b0 - fst a0
  v1 = fst vb - fst va
  p2 = snd a0 - snd x0
  q2 = snd va - snd vx
  u2 = snd b0 - snd a0
  v2 = snd vb - snd va
  a' = q1*v2 - q2*v1
  b' = p1*v2 + q1*u2 - p2*v1 - q2*u1
  c' = p1*u2 - p2*u1
  answer | a' == 0 = OneSolution (-c' / b')
         | otherwise = quadraticFormula a' b' c'
  redact t = between t0 t1 t && between 0 1 s {- && extraValidity -} where
    s = if u1 == 0 && v1 == 0
          then (-p2 - q2*t)/(u2 + v2*t)
          else (-p1 - q1*t)/(u1 + v1*t)
    --extraValidity = (u1 + v1*t /= 0) && (u2 + v2*t /= 0)
    --this shouldnt come up unless a=0, in which case division by zero never happened

quadraticFormula :: R -> R -> R -> Solution
quadraticFormula a b c =
  let twoA = 2*a in
  let minusB = -b in
  let discr = b*b - 4*a*c in
  if discr < 0
    then NoSolution
    else if discr == 0
      then OneSolution (minusB / twoA)
      else TwoSolutions
        ((minusB + sqrt discr)/twoA)
        ((minusB - sqrt discr)/twoA)

filterSol :: (R -> Bool) -> Solution -> Solution
filterSol f sol = case sol of
  NoSolution -> sol
  OneSolution x -> if f x then sol else NoSolution
  TwoSolutions x y -> case (f x, f y) of
    (True,True) -> sol
    (True,False) -> OneSolution x
    (False,True) -> OneSolution y
    _ -> NoSolution

between :: R -> R -> R -> Bool
between l r x = l <= x && x <= r

firstSolution :: Solution -> Maybe R
firstSolution sol = case sol of
  NoSolution -> Nothing
  OneSolution x -> Just x
  TwoSolutions x y -> Just (min x y)
