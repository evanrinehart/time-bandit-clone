{-# LANGUAGE DeriveFunctor #-}
module Dispatcher where

import Prelude hiding (filter)
import qualified Prelude as P (filter)

-- each rule is either waiting indefinitely, or it is inline exactly once in
-- the dispatcher to run a command of type Poke s (). the operations required
-- of the dispatcher are: insert a new (Time, ruleId, command) into the queue, advance
-- the current time in the queue to some point, advance the current time to
-- the next command, and cancel some event by ruleId.


data Disp t a = Disp !t [(t, a)] deriving (Show, Functor)

empty :: t -> Disp t a
empty t0 = Disp t0 []

insert :: (Num t, Ord t) => t -> a -> Disp t a -> Disp t a
insert dt x' (Disp t es) = Disp t (go (t + dt) es) where
  go t' [] = [(t', x')]
  go t' es@((t,x):es') | t' <= t = (t',x') : es
                       | otherwise = (t,x) : go t' es'

advance :: (Num t, Ord t) => t -> Disp t a -> (Maybe (a,t), Disp t a)
advance dt (Disp t []) = (Nothing, Disp (t + dt) [])
advance dt (Disp t es@((t',x):es'))
  | t + dt < t' = (Nothing, Disp (t + dt) es)
  | otherwise = (Just (x, t'-t), Disp t' es')

deleteBy :: (a -> Bool) -> Disp t a -> Disp t a
deleteBy f (Disp t es) = Disp t (P.filter (not . f . snd) es)

eta :: Num t => Disp t a -> Maybe t
eta (Disp t []) = Nothing
eta (Disp t ((t',_):_)) = Just (t' - t)


