module Segment where

-- a data structure for concatenating animation segments in time

data Segs m a = End (A m a) | Segs Time (A m a) (m -> Segs m a)

instance (Show m, Show a) => Show (Segs m a) where
  show (End a) = "End (" ++ show a ++ ")"
  show (Segs t a mkss) = "Segs (" ++ show t ++ " " ++ show a ++ " ; ... )"

peek :: Segs m a -> a
peek (End a) = view a
peek (Segs t a _) = view a

segments :: Segs m v -> A (Segs m v) v
segments segs = A segs peek go where
  go dt (End a) = let a' = advance dt a in A (End a') peek go
  go dt (Segs t a mkss) = if dt < t
    then A (Segs (t - dt) (advance dt a) mkss) peek go
    else
      let A m _ _ = advance t a in
      go (dt - t) (mkss m)

seg :: (A m v -> A m v) -> Segs m v -> Segs m v
seg f (End a) = End (f a)
seg f (Segs t a ss) = Segs t (f a) ss

