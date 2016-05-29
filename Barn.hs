{-# LANGUAGE DeriveFunctor #-}
module Barn where

import Data.IntMap as IM
import Data.Fixed

import Animation
import Path
import Motion
import Types
import Grid

data Motion =
  Stationary GridIx Dir |
  Motion (R2,R2) [(R2,R2)] (R2,R2) GridIx Dir
    deriving Show

data BarnElem m a = BarnElem m a deriving (Show,Functor)
data Barn a = Barn
  { barnCounter :: Int
  , barnMap :: IntMap (BarnElem Motion a)
  } deriving (Show,Functor)

current :: Motion -> R2
current (Motion (x,_) _ _ _ _) = x
current (Stationary (x,y) _) = (realToFrac x, realToFrac y)

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

stopMotion :: Motion -> Motion
stopMotion (Motion xv paths xv' gix d) = Stationary gix d

--norm (x,y) = sqrt (x*x + y*y)
diff (a,b) (c,d) = (a-c,b-d)
dumb dt ((x1,x2),(v1,v2)) = ((x1 + v1*dt, x2 + v2*dt),(v1,v2))

launch :: Dir -> GridIx -> (R2,R2) -> Motion
launch dir gix xv = Motion xv [] xv gix dir

motion' :: Delta -> (R2,R2) -> ([(R2,R2)], (R2,R2))
motion' = motion norm diff id 5 dumb

linear :: R -> Motion -> Motion
linear _ mo@(Stationary _ _) = mo
linear dt (Motion _ _ xv ix dir) =
  let (ps, xv') = motion' dt xv in Motion xv ps xv' ix dir

barnThings :: Path (Barn a) (IntMap (BarnElem Motion a))
barnThings = Path (w8 1) (Just . barnMap) (\f (Barn x y) -> Barn x (f y))

barn :: Anim a -> Anim (Barn a)
barn go dt (Barn c im) = Barn c (fmap (elemA go dt) im)

elemA :: Anim a -> Anim (BarnElem Motion a)
elemA go dt (BarnElem mo x) = BarnElem (linear (realToFrac dt) mo) (go dt x)

insertThing :: Motion -> a -> Barn a -> Barn a
insertThing mo item (Barn c im) = Barn (c+1) im' where
  im' = IM.insert c (BarnElem mo item) im

deleteThing :: Int -> Barn a -> Barn a
deleteThing k (Barn c im) = Barn c' im' where
  c' = if k == c-1 then recycle c im else c
  im' = IM.delete k im

recycle :: Int -> IntMap a -> Int
recycle k im = if IM.member (k-1) im then k else recycle (k-1) im

emptyBarn :: Barn a
emptyBarn = Barn 0 IM.empty
