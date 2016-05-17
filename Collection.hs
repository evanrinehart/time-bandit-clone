{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Collection where

-- ways to animate collections, work in progress

import Data.Bifunctor
import qualified Data.Map as M

import Animation

collect :: (forall a b . (a -> b) -> f a -> f b) -> f (A m v) -> A (f (A m v)) (f v)
collect map start = A start (map view) go where
  go dt m = let m' = map (advance dt) m in A m' (map view) go

collect2 :: (forall a b c d . (a -> c) -> (b -> d) -> f a b -> f c d) ->
            f (A m1 v1) (A m2 v2) ->
            A (f (A m1 v1) (A m2 v2)) (f v1 v2) 
collect2 map start = A start (map view view) go where
  go dt m = let m' = map (advance dt) (advance dt) m in A m' (map view view) go

barn :: M.Map Int (A m v) -> A (M.Map Int (A m v)) (M.Map Int v)
barn = collect M.map

fuse :: A m1 v1 -> A m2 v2 -> A (A m1 v1, A m2 v2) (v1,v2)
fuse a1 a2 = collect2 bimap (a1,a2)

lift :: Functor f => f (A m v) -> A (f (A m v)) (f v)
lift = collect fmap

