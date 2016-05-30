{-# LANGUAGE DeriveFunctor #-}
module Zipper where

import Path

data Zipper a = Zipper [a] a [a] deriving Functor

-- showsPrec :: Int -> Zipper a -> String -> String

instance Show a => Show (Zipper a) where
  showsPrec d (Zipper l x r) =
    showParen (d > 10) $
    showString "Zipper " .
    (if null l then id else showString ".. ") .
    showsPrec 11 x . 
    if null r then id else showString " .. "

singleton :: a -> Zipper a
singleton x = Zipper [] x []

fromList :: [a] -> Zipper a
fromList [] = error "empty zipper"
fromList (g:gs) = Zipper [] g gs

cycle :: [a] -> Zipper a
cycle [] = error "empty zipper"
cycle (g:gs) = Zipper (go gs' gs') g (go gs gs) where
  gs' = reverse gs
  go loop (g:gs) = g : go loop gs
  go loop [] = loop

next :: Zipper a -> Zipper a
next (Zipper xs y []) = Zipper xs y []
next (Zipper xs y (z:zs)) = Zipper (y:xs) z zs

prev :: Zipper a -> Zipper a
prev (Zipper [] y zs) = Zipper [] y zs
prev (Zipper (x:xs) y zs) = Zipper xs x (y:zs)

current :: Zipper a -> a
current (Zipper _ x _) = x

_current :: Path (Zipper a) a
_current = Path (w8 0) (Just . current) s where
  s f (Zipper l x r) = Zipper l (f x) r
