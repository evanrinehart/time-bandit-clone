module LinePoint where

import Debug.Trace

-- solve the following problem
-- a point moves along a linear path
-- x = x0 + vx*t where t in [t0, t1]
-- a line segment moves, its end points move along linear paths
-- a = a0 + va*t where t in [t0, t1]
-- b = b0 + vb*t
-- seg = a + (b - a)*s where s in [0,1]
-- solve for 0 1 or 2 times in [t0,t1] that the point crosses the line
-- if the point is on an overlapping parallel path with the line, no solution

type R = Double
type R2 = (R,R)

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

firstSolution :: Solution -> Maybe R
firstSolution sol = case sol of
  NoSolution -> Nothing
  OneSolution x -> Just x
  TwoSolutions x y -> Just (min x y)

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
  redact t = between t0 t1 t && between 0 1 s && extraValidity where
    s = if u1 == 0 && v1 == 0
          then (-p2 - q2*t)/(u2 + v2*t)
          else (-p1 - q1*t)/(u1 + v1*t)
    extraValidity = (u1 + v1*t /= 0) && (u2 + v2*t /= 0)

debug :: Show a => a -> b -> b
debug x y = Debug.Trace.trace (show x) y

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
