{-# LANGUAGE DeriveFunctor #-}
module Barn where

import Data.IntMap as IM
import Data.Fixed

import Animation
import Path
import Motion
import Types
import Grid

data BarnElem m a = BarnElem m a deriving (Show,Functor)
data Barn a = Barn
  { barnCounter :: Int
  , barnMap :: IntMap (BarnElem Motion a)
  } deriving (Show,Functor)

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
