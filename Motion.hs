module Motion where

motion :: (Fractional dt, Floating r, Ord r)
       => (v2 r -> r)
       -> (v2 r -> v2 r -> v2 r)
       -> (m -> (v2 r, v2 r))
       -> r
       -> (dt -> m -> m)
       -> dt
       -> m
       -> ([(v2 r, v2 r)], m)
motion norm diff inspect rez f dt0 m0 = rev1 $ go m0 [] dt0 where
  rev1 (ps,m) = (reverse ps, m)
  go m ps dt =
    let m' = f dt m in
    let p = inspect m' in
    let (x,x') = (fst . inspect $ m, fst . inspect $ m') in
    if norm (diff x' x) < rez
      then (p:ps, m')
      else
        let (ps', m') = go m ps (dt/2) in
        go m' ps' (dt/2)

