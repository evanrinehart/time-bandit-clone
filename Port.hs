module Port where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Poke

data Port a = Port (TVar Bool) (TVar Int)

newPort :: IO (Port a)
newPort = Port <$> newTVarIO False <*> newTVarIO (-1)

activatePort :: Int -> Port a -> IO ()
activatePort c p@(Port tv1 tv2) = do
  b <- portIsActive p
  when b (throwIO (userError "attempting to activate an active port"))
  atomically $ do
    writeTVar tv1 True
    writeTVar tv2 c

portIsActive :: Port a -> IO Bool
portIsActive (Port tv1 _) = atomically (readTVar tv1)
