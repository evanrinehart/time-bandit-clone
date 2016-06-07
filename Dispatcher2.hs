{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Dispatcher2 where

import Data.HashMap.Strict (HashMap, foldlWithKey')
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List hiding (insert)
import Data.Ord

-- the dispatcher is a priority queue for upcoming (predicted) events.
-- the entries in the dispatcher are actions to apply to the game state.
-- the keys are time, prediction pairs. the main operations are dequeuing
-- the next event, inserting a new event, and removing all events for a given
-- prediction.

type Horrible t k a = HashMap k (t,[((Int,Int),a)])
data Disp2 t k a = MkDisp !t (HashMap k (t,[((Int,Int),a)]))
  deriving (Show,Functor)

empty :: Num t => Disp2 t k a
empty = MkDisp 0 H.empty

insert :: (Eq k, Hashable k, Num t)
       => k
       -> t
       -> [((Int,Int),a)]
       -> Disp2 t k a
       -> Disp2 t k a
insert !k !dt entries (MkDisp c hm) =
  MkDisp c (H.insert k (c + dt, sortBy (comparing fst) entries) hm)

delete :: (Eq k, Hashable k) => k -> Disp2 t k a -> Disp2 t k a
delete k (MkDisp c hm) = MkDisp c (H.delete k hm)

advance :: (Ord dt, Num dt, Eq k, Hashable k)
        => dt
        -> Disp2 dt k a
        -> Either (Disp2 dt k a) (dt, dt, k, a, Disp2 dt k a)
advance dt d@(MkDisp c hm) = case minTime hm of
  Nothing -> Left (MkDisp (c + dt) hm)
  Just t | c + dt < t -> Left (MkDisp (c + dt) hm)
         | otherwise  ->
           let kijas = entriesAtTime t hm in
           let (k,_,x) = minimumBy (comparing ordField) kijas in
           let d' = MkDisp t (H.update drop1 k hm) in
           Right (t - c, dt - (t - c), k, x, d')

entriesAtTime :: (Eq t) => t -> Horrible t k a -> [(k,(Int,Int),a)]
entriesAtTime t hm = foldlWithKey' f [] hm where
  f kijas k (t',(ij,a):_) | t == t' = (k,ij,a):kijas
                          | otherwise = kijas
  f kijas k (t',_) = error "empty list in dispatcher"

ordField :: (k,(Int,Int),a) -> (Int,Int)
ordField (x,y,z) = y

drop1 :: (t,[a]) -> Maybe (t,[a])
drop1 (t,[x]) = Nothing
drop1 (t,x:xs) = Just (t,xs)

minTime :: (Ord t) => HashMap k (t,b) -> Maybe t
minTime hm = foldl' f Nothing hm where
  f Nothing (t,_) = Just t
  f (Just t1) (t2,_) = Just (min t1 t2)

