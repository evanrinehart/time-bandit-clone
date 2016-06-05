{-# LANGUAGE BangPatterns #-}
module Dispatcher2 where

import Data.IntMap.Strict (IntMap, foldlWithKey')
import qualified Data.IntMap.Strict as IM
--import Data.List 

-- the dispatcher is a priority queue for upcoming (predicted) events.
-- the entries in the dispatcher are actions to apply to the game state.
-- the keys are time, prediction pairs. the main operations are dequeuing
-- the next event, inserting a new event, and removing all events for a given
-- prediction.

data Disp2 t a = MkDisp !t (IntMap (t, a))
  deriving Show

empty :: Num t => Disp2 t a
empty = MkDisp 0 IM.empty

insert :: Num t => Int -> t -> a -> Disp2 t a -> Disp2 t a
insert !k !dt !x (MkDisp c im) = MkDisp c (IM.insert k (c + dt, x) im)

delete :: Int -> Disp2 t a -> Disp2 t a
delete k (MkDisp c im) = MkDisp c (IM.delete k im)

next :: (Num t, Ord t) => Disp2 t a -> Maybe (t, Int)
next (MkDisp c im) = fixup <$> foldlWithKey' f Nothing im where
  fixup (t,k) = (t - c, k)
  f Nothing k (dt,_) = Just (dt,k)
  f accum@(Just (dt0,k0)) k (dt,_)
    | dt < dt0 = Just (dt,k)
    | otherwise = accum

advance :: Num t => t -> Disp2 t a -> Disp2 t a
advance dt (MkDisp c im) = MkDisp (c+dt) im

dequeue :: (Num t, Ord t) => Disp2 t a -> Maybe (a, t, Disp2 t a)
dequeue d@(MkDisp c im) = f <$> next d where
  f (_, k) = (x, t-c, MkDisp t im') where
    (Just (t,x), im') = IM.updateLookupWithKey (\_ _ -> Nothing) k im

