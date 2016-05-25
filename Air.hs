{-# LANGUAGE DeriveFunctor #-}
module Air where

import Animation
import Path
import Clock

data Air dt = HaveAir | AirRunningOut (Clock dt ()) deriving Show

air :: Num dt => A dt (Air dt)
air dt (AirRunningOut cl) = AirRunningOut (clock (const id) dt cl)
air dt HaveAir = HaveAir

airTimeLeft :: (Num dt, Ord dt) => Path (Air dt) dt
airTimeLeft = Path (w8 1) g p where
  g HaveAir = Nothing
  g (AirRunningOut (Clock c _ _)) = if c > 0 then Just c else Nothing
  p f (AirRunningOut (Clock c r ())) = AirRunningOut (Clock (f c) r ())
  p f HaveAir = HaveAir
