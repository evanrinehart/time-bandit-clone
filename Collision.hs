{-# LANGUAGE TupleSections #-}
module Collision where

import Data.Map (Map)
import Data.IntMap (IntMap, (!))
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad
import Data.Maybe
import Data.List
import Types
import Motion as MO
import LinePoint

import Animation
import Path
import Facade

-- a collision index for predicting time of collision between two classes

data ColIndex t a b = MkCix
  { colIx    :: Map a (Maybe (b,t))
  , colIxRev :: Map b [a]
  , colIxMin :: Maybe (t,a,b)
  , colIxClk :: t
  } deriving Show

-- the collision index animates like a clock
-- in the future we can reset all times in the index periodically
colIndex :: Num t => A t (ColIndex t a b)
colIndex dt ix = ix { colIxClk = colIxClk ix + dt }

colIxFacade :: (Eq t, Eq a, Eq b, Num t)
            => ColIndex t a b -> Facade (ColIndex t a b) (Maybe (t,a,b))
colIxFacade ix = Facade v (==) ix where
  v (MkCix _ _ xmin clk) = fmap (\(t,a,b) -> (t - clk, a, b)) xmin

targetSize = 0.75

empty :: Num t => ColIndex t a b
empty = MkCix M.empty M.empty Nothing 0

-- check for soonest collision with targets. if there is one, add it to
-- targets index. if sooner than min, update min.
insertMissile :: (Ord a)
              => a -> R2 -> R2 -> IntMap Motion
              -> ColIndex Time a Int -> ColIndex Time a Int
insertMissile k x v targets (MkCix cix rcix xmin clk) =
  MkCix cix' rcix' xmin' clk where
    prediction = missileCheckTargets x v targetSize targets
    cix' = M.insert k prediction cix
    rcix' = maybe rcix updateTarget prediction
    updateTarget (targ, _) = M.adjust (`union` [k]) targ rcix
    xmin' = maybe xmin (\(targ,t) -> Just (t,k,targ)) prediction
    check (targ,t) = case xmin of
      Nothing -> Just (t,k,targ)
      Just (t0,_,_) -> if t < t0 then Just (t,k,targ) else xmin
      

-- remove the missile. if it had a target, remove it from targets index.
-- if it was the min, recompute min.
removeMissile :: (Ord a, Ord b, Ord t)
              => a -> ColIndex t a b -> ColIndex t a b
removeMissile k (MkCix cix rcix xmin clk) = MkCix cix' rcix' xmin' clk where
  affectedTarget = fmap fst =<< M.lookup k cix
  cix' = M.delete k cix
  rcix' = maybe rcix (\targ -> M.adjust (delete k) targ rcix) affectedTarget
  xmin' = case xmin of
    Nothing -> Nothing
    Just (t,k',targ') | k == k'   -> computeMin cix'
                      | otherwise -> xmin

-- check for any collisions with missiles. for each missile, update their
-- entry. for their previous targets, remove them from the target index.
-- if the soonest collision is less than min, update min.
insertTarget :: Int -> R2 -> R2 -> IntMap Motion
             -> ColIndex Time Int Int -> ColIndex Time Int Int
insertTarget targ x v missiles ix@(MkCix cix rcix xmin clk) = ix' where
  collisions = targetCheckMissiles x v targetSize missiles
  ixWithNewTarget = MkCix cix (M.insert targ [] rcix) xmin clk
  ix' = foldl' housekeeping ixWithNewTarget collisions
  housekeeping ix (k, t) = redirectMissile k (Just (targ,t)) ix

-- remove target. for all missiles that would hit target, recompute their
-- predictions, and update the appropriate new targets indexes. if any affected
-- missiles were the min, recompute the min.
removeTarget :: Int -> IntMap Motion -> IntMap Motion
             -> ColIndex Time Int Int -> ColIndex Time Int Int
removeTarget targ missiles targets ix@(MkCix cix rcix xmin clk) = ix' where
  (Just missileKeys, rcix') = M.updateLookupWithKey f targ rcix
  f _ _ = Nothing
  ixWithoutTarget = MkCix cix rcix' xmin clk
  ix' = foldl' housekeeping ixWithoutTarget missileKeys
  housekeeping ix k = ix' where
    mo = missiles ! k
    x = MO.current mo
    v = MO.currentVel mo
    ix' = redirectMissile k (missileCheckTargets x v targetSize targets) ix

redirectMissile :: (Ord a, Ord b, Ord t)
                => a -> Maybe (b,t) -> ColIndex t a b -> ColIndex t a b
redirectMissile k mtarg (MkCix cix rcix xmin clk) = case mtarg of
  Nothing -> MkCix cix' rcix' xmin' clk where
    cix' = M.insert k Nothing cix
    rcix' = maybe rcix remove (fmap fst =<< M.lookup k cix)
    remove targ = M.adjust (delete k) targ rcix
    xmin' = case xmin of
      Nothing -> Nothing
      Just (t0,k0,_) | k == k0 -> computeMin cix'
                     | otherwise -> xmin
  Just (targ,t) -> MkCix cix' rcix' xmin' clk where
    cix' = M.insert k (Just (targ,t)) cix
    rcix' = case fmap fst =<< M.lookup k cix of
      Nothing -> M.adjust (`union` [k]) targ rcix
      Just targ0 ->
        M.adjust (`union` [k]) targ . M.adjust (delete k) targ0 $ rcix
    xmin' = case xmin of
      Nothing -> Just (t,k,targ)
      Just (t0,k0,_) | k == k0 -> if t <= t0
                         then Just (t,k,targ)
                         else computeMin cix'
                     | otherwise -> if t < t0
                         then Just (t,k,targ)
                         else xmin

-- check for soonest collision with a set of targets, if any
missileCheckTargets :: R2 -> R2 -> R -> IntMap Motion -> Maybe (Int,Time)
missileCheckTargets x v tsize targets = IM.foldrWithKey' f Nothing targets where
  f k mo Nothing = (k,) <$> missileCheck x v tsize mo
  f k mo a@(Just (k0,t0)) = case missileCheck x v tsize mo of
    Nothing -> a
    Just t -> if t < t0 then Just (k,t) else a

-- check for collision between missile and moving rectangle of size tsize
missileCheck :: R2 -> R2 -> R -> Motion -> Maybe Time
missileCheck mx mv tsize mo = foldl' soonest Nothing results where
  results = map (firstSolution . solve) [s1,s2,s3,s4]
  (x,y) = MO.current mo
  segV = MO.currentVel mo
  inf = 1/0
  half = tsize / 2
  pr x0 x1 = Problem mx mv x0 segV x1 segV 0 inf
  s1 = pr (x+half,y+half) (x+half,y-half)
  s2 = pr (x-half,y+half) (x-half,y-half)
  s3 = pr (x+half,y+half) (x-half,y+half)
  s4 = pr (x+half,y-half) (x-half,y-half)
  soonest Nothing Nothing = Nothing
  soonest (Just t) (Just t0) = Just (min t t0)
  soonest Nothing x = x
  soonest x Nothing = x

-- check for soonest collision with a set of missiles, if any
targetCheckMissiles :: R2 -> R2 -> R -> IntMap Motion -> [(Int, Time)]
targetCheckMissiles x v tsize missiles = mapMaybe f (IM.toList missiles) where
  mo = moKludge x v
  f (k,mo') = (k,) <$> missileCheck (MO.current mo') (MO.currentVel mo') tsize mo
  
moKludge x v = let u = undefined in Motion (x,v) u u u u

computeMin :: Ord t => Map a (Maybe (b,t)) -> Maybe (t,a,b)
computeMin cix = M.foldrWithKey' f Nothing cix where
  f _ Nothing Nothing                     = Nothing
  f k (Just (target,t)) a@(Just (t0,_,_)) =
    if t < t0
      then Just (t,k,target)
      else a
  f _ Nothing a                           = a
  f k (Just (target, t)) Nothing          = Just (t, k, target)
