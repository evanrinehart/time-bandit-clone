{-# LANGUAGE BangPatterns #-}
module Port where

import Data.IORef
import Control.Exception
import Control.Monad
import System.IO.Unsafe
import Unsafe.Coerce
import System.Mem.StableName

data Port a = Port { portObjectIdentity :: IORef () }

instance Show (Port a) where
  show _ = "<Port>"

newPort :: IO (Port a)
newPort = do
  ref <- newIORef ()
  return (Port ref)

-- the justification for this is more shaky than predCoerce. Ports have
-- object identity so it would be very wrong for this to give a positive
-- result if the ports are different, same type or not. The port contains
-- a unique IORef to ensure that ports created with different IO actions
-- are different objects.
portCoerce :: Port a -> Port b -> a -> Maybe b
portCoerce !p1 !p2 x = unsafePerformIO $ do
  n1 <- makeStableName p1
  n2 <- makeStableName p2
  if eqStableName n1 n2
    then return (Just (unsafeCoerce x))
    else return Nothing
