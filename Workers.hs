{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Workers where

import Control.Monad (forever)
import Control.Concurrent
import System.Timeout
import Control.Exception
import Data.List (delete)

type Workers = MVar [ThreadId]

spawnRequest :: forall a .
                Workers
             -> Maybe Int
             -> IO a
             -> (forall e . Exception e => Either e a -> IO ())
             -> IO ()
spawnRequest mv mtimeout io handler = do
  let wrapper = maybe (fmap Just) timeout mtimeout
  tid <- forkFinally (wrapper io) $ \mresult -> do
    tid <- myThreadId
    modifyMVar_ mv (return . delete tid)
    case mresult of
      Left e -> handler (Left e)
      Right Nothing -> handler (Left (userError "timeout"))
      Right (Just x) -> handler (Right x :: Either SomeException a)
  modifyMVar_ mv (return . (tid:))

spawnInputHandler :: String -> Workers -> IO a -> (a -> IO ()) -> IO ()
spawnInputHandler label mv io h = do
  tid <- forkFinally (forever (io >>= h)) $ \e -> do
    putStr ("input worker "++label++" is dead: ")
    case e of
      Left e -> print e
      Right _ -> putStrLn "terminated normally (impossible)"
    tid <- myThreadId
    modifyMVar_ mv (return . delete tid)
  modifyMVar_ mv (return . (tid:))

newWorkers :: IO Workers
newWorkers = newMVar []

clearWorkers :: Workers -> IO ()
clearWorkers mv = withMVar mv (mapM_ killThread)
