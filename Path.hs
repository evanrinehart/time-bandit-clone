module Path where

import Prelude hiding ((.), id)
import Control.Category
import Data.Monoid

import Bag

type Edit s a = (a -> a) -> s -> s
data Path s a = Path (Bag Integer) (s -> a) (Edit s a)

get :: Path s a -> s -> a
get (Path _ g _) x = g x
set :: Path s a -> (a -> a) -> s -> s
set (Path _ _ e) f x = e f x
crumbs :: Path s a -> [Integer]
crumbs (Path p _ _) = toList p

left :: Path (a, b) a
left = Path (single 0) fst (\f (x,y) -> (f x, y))

instance Category Path where
  id = Path mempty id (const id)
  (Path p1 g1 e1) . (Path p2 g2 e2) = Path (p1 <> p2) (g1 . g2) (e2 . e1)
