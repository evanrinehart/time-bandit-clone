{-# LANGUAGE DeriveFunctor #-}
module GameOver where

import Animation
import Path

data GameOver a = GameOver !a | Playing !a deriving (Functor, Show)

ungameover :: GameOver a -> a
ungameover (GameOver x) = x
ungameover (Playing x) = x

playing' :: Path (GameOver a) a
playing' = Path (w8 0) g s where
  g (Playing x)  = Just x
  g (GameOver _) = Nothing
  s f (Playing x) = Playing (f x)
  s f y = error "game over"

gameover :: A dt a -> A dt (GameOver a)
gameover go dt (GameOver x) = GameOver x
gameover go dt (Playing x) = Playing (go dt x)
