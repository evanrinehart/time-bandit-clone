{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Animation where

import Control.Concurrent (threadDelay)
import Data.Fixed
import Data.Bifunctor
import Control.Applicative (liftA2)

type A dt a = dt -> a -> a

isoMap :: (a -> b) -> (b -> a) -> A dt a -> A dt b
isoMap f g go dt y = f (go dt (g y))

wrap :: Functor f => A dt a -> A dt (f a)
wrap = (fmap .)

fuse :: Bifunctor f => A dt a -> A dt b -> A dt (f a b)
fuse = liftA2 bimap

testA :: (Show a, Fractional dt) => A dt a -> a -> IO ()
testA f x = do
  print x
  let x' = f 0.1 x
  threadDelay 100000
  testA f x'
