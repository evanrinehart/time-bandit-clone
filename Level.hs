module Level where

import Grid as G

type Level a = Zipper (Grid a)
data Zipper a = Zipper [a] a [a]

instance Show a => Show (Zipper a) where
  show (Zipper _ x _) = "Zipper _ "++show x++" _"

fromList :: [Grid a] -> Level a
fromList [] = error "empty level"
fromList (g:gs) = Zipper [] g gs

cycle :: [Grid a] -> Level a
cycle [] = error "empty level"
cycle (g:gs) = Zipper (go gs' gs') g (go gs gs) where
  gs' = reverse gs
  go loop (g:gs) = g : go loop gs
  go loop [] = loop

next :: Level a -> Level a
next (Zipper xs y []) = Zipper xs y []
next (Zipper xs y (z:zs)) = Zipper (y:xs) z zs

prev :: Level a -> Level a
prev (Zipper [] y zs) = Zipper [] y zs
prev (Zipper (x:xs) y zs) = Zipper xs x (y:zs)

