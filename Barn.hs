{-# LANGUAGE DeriveFunctor #-}
module Barn where


import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
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

elemMotion :: BarnElem m a -> m
elemMotion (BarnElem m _) = m

elemPayload :: BarnElem m a -> a
elemPayload (BarnElem _ x) = x

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
deleteThing k (Barn c im) = recycle (Barn c (IM.delete k im)) where
  recycle b@(Barn c im) | c < 0 = Barn 0 im
                        | IM.member (c-1) im = b
                        | otherwise = recycle (Barn (c-1) im)

emptyBarn :: Barn a
emptyBarn = Barn 0 IM.empty

count :: Barn a -> Int
count (Barn _ im) = IM.size im

toList :: Barn a -> [BarnElem Motion a]
toList (Barn _ im) = map snd $ IM.toList im

foldr :: (Int -> a -> b -> b) -> b -> Barn a -> b
foldr f m0 (Barn _ im) = IM.foldrWithKey g m0 im where
  g k (BarnElem _ pay) accum = f k pay accum
