module Examples where

import Control.Concurrent
import Control.Monad
import Text.Show.Functions
import qualified Data.Map as M

import Animation
import Clock
import Physics
import Newtonian
import Collection
import PingPong
import Asteroids
import Alt

circle :: A Clock R2
circle = fmap ((\t -> (cos t, sin t)) . realToFrac) (clock 0 1)

test :: (Show m, Show v) => A m v -> IO ()
test a = do
  let micros = 1000000 `div` 10
  let dt = realToFrac micros / 1000000
  threadDelay micros
  print a
  test (advance dt a)

left :: (a -> b) -> (a,c) -> (b,c)
left f (x,y) = (f x, y)

right :: (a -> b) -> (c,a) -> (c,b)
right f (x,y) = (x, f y)

key :: Ord k => k -> (a -> a) -> M.Map k a -> M.Map k a
key k f = M.adjust f k


