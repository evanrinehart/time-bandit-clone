module Detect where

-- an animation that continues until an event is detected, then it
-- mutates

data DetectTime a = NotBefore Time | At Time a deriving Show

detect :: (m -> DetectTime a) -> (a -> m -> m) -> A m v -> A m v
detect d f (A m view go0) = A m view (go go0) where
  check x@(NotBefore t) | t < 0 = error ("NotBefore " ++ show t ++ " invalid")
                        | otherwise = x
  check x@(At t _) | t < 0 = error ("At " ++ show t ++ " invalid")
                   | otherwise = x
  go innerGo dt m = case check (d m) of
    NotBefore t -> if dt < t
      then let A m' view' go' = innerGo dt m in A m' view' (go go')
      else let A m' view' go' = innerGo t m in go go' (dt - t) m'
    At t x -> if dt < t
      then let A m' view' go' = innerGo dt m in A m' view' (go go')
      else
        let A m' view' go' = innerGo t m in
        let m'' = f x m' in
        go go' (dt - t) m''

