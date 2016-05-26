{-# LANGUAGE DeriveFunctor #-}
module Barn where

import Data.IntMap as IM
import Data.Fixed

import Animation
import Path
import Motion
import Types

type GridIx = (Int,Int)
data Motion =
  Stationary R2 GridIx |
  Motion (R2,R2) [(R2,R2)] (R2,R2) GridIx
    deriving Show

data BarnElem m a = BarnElem m a deriving (Show,Functor)
data Barn a = Barn
  { barnCounter :: Int
  , barnMap :: IntMap (BarnElem Motion a)
  } deriving (Show,Functor)

current :: Motion -> R2
current (Motion (x,_) _ _ _) = x

final :: Motion -> (R2,R2)
final (Motion _ _ xv _) = xv

--norm (x,y) = sqrt (x*x + y*y)
diff (a,b) (c,d) = (a-c,b-d)
dumb dt ((x1,x2),(v1,v2)) = ((x1 + v1*dt, x2 + v2*dt),(v1,v2))

motion' :: Delta -> (R2,R2) -> ([(R2,R2)], (R2,R2))
motion' = motion norm diff id 5 dumb

linear :: R -> Motion -> Motion
linear _ mo@(Stationary _ _) = mo
linear dt (Motion _ _ xv ix) =
  let (ps, xv') = motion' dt xv in Motion xv ps xv' ix

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
