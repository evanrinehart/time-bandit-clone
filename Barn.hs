{-# LANGUAGE DeriveFunctor #-}
module Barn where

import Data.IntMap as IM
import Data.Fixed

import Animation
import Path
import Motion
import Types

type Anim a = A Pico a

data Motion = Motion (R2,R2) [(R2,R2)] (R2,R2) deriving Show
type Square = (Int,Int,Int)

current :: Motion -> R2
current (Motion (x,_) _ _) = x

final :: Motion -> (R2,R2)
final (Motion _ _ x) = x

--norm (x,y) = sqrt (x*x + y*y)
diff (a,b) (c,d) = (a-c,b-d)
dumb dt ((x1,x2),(v1,v2)) = ((x1 + v1*dt, x2 + v2*dt),(v1,v2))
motion' = motion norm diff id 5 dumb

linear :: R -> Motion -> Motion
linear dt mo =
  let xv = final mo in
  let (ps, xv') = motion' dt xv in
  Motion xv ps xv'

data BarnElem a = BarnElem Motion Square a deriving (Show,Functor)
data Barn a = Barn
  { barnCounter :: Int
  , barnMap :: IntMap (BarnElem a)
  } deriving (Show,Functor)

barnThings :: Path (Barn a) (IntMap (BarnElem a))
barnThings = Path (w8 1) (Just . barnMap) (\f (Barn x y) -> Barn x (f y))

barn :: Anim a -> Anim (Barn a)
barn go dt (Barn c im) = Barn c (fmap (elemA go dt) im)

elemA :: Anim a -> Anim (BarnElem a)
elemA go dt (BarnElem mo sq x) = BarnElem (linear (realToFrac dt) mo) sq (go dt x)

insertThing :: R2 -> R2 -> Square -> a -> Barn a -> Barn a
insertThing x v sq item (Barn c im) = Barn (c+1) im' where
  mo = let (ps, m') = motion' 0 (x,v) in Motion (x,v) ps m'
  im' = IM.insert c (BarnElem mo sq item) im

deleteThing :: Int -> Barn a -> Barn a
deleteThing k (Barn c im) = Barn c' im' where
  c' = if k == c-1 then recycle c im else c
  im' = IM.delete k im

recycle :: Int -> IntMap a -> Int
recycle k im = if IM.member (k-1) im then k else recycle (k-1) im

emptyBarn :: Barn a
emptyBarn = Barn 0 IM.empty

example :: (Anim (Barn ()), Barn ())
example = (barn (const id), insertThing (0,0) (1,2) (0,0,0) () emptyBarn)
