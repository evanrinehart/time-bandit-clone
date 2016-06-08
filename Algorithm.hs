{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithm where

import Prelude hiding ((.),id)
--import qualified Data.IntMap as IM
--import Data.IntMap (IntMap)
import Data.List
--import qualified Data.IntSet as IS
--import Data.IntSet (IntSet)
import Control.Concurrent
import Control.Exception
--import Data.Fixed
import Control.Monad (forM_)
import Control.Monad.State
import Data.Trie as T
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.ByteString (ByteString)

import Animation as A
import Path
import Plan
import Poke
import Port
import Prediction
import Event as E
import Dispatcher2 as D
import DepIndex2 as DI
--import Workers as W
import Viewing

import Control.Monad (when)
import Control.Category

type Rule dt s = Event dt s (Poke s ())

data Sim dt s = Sim
  { simModel :: !s
  , simAnim  :: A dt s
  , simRules :: [Rule dt s]

  , simPreds :: HashMap PrName (AnyPred dt s)
  , simDisp  :: Disp2 dt PrName (Poke s ())
  , simIndex :: DepIx2 PrName ByteString
  , simCounter :: Int
  } 

data Interface dt s = Interface
   { simWait      :: dt -> IO ()
   , simRender    :: forall a . (s -> a) -> IO a
   , simWritePort :: forall a . Port a -> a -> IO ()
   , simKill      :: IO ()
   , simDebug     :: IO (Sim dt s)
   }

instance Show (Interface dt s) where
  show _ = "<Interface>"

instance (Show dt, Show s) => Show (Sim dt s) where
  showsPrec d sim =
    showParen (d > 10) $
    showString "Sim " .
    showsPrec 11 (simModel sim) . 
    showString " " .
    showsPrec 11 (fmap (const "<poke>") (simDisp sim)) .
    showString " " .
    showsPrec 11 (simIndex sim) .
    showString " " .
    showsPrec 11 (simCounter sim)

takeCounter :: State (Sim dt s) Int
takeCounter = do
  c <- gets simCounter
  modify (\sim -> sim { simCounter = c + 1 })
  return c

shiftDispatcher :: (Num dt, Ord dt)
                => dt -> State (Sim dt s) (Maybe (PrName,Poke s (),dt,dt))
shiftDispatcher dt = do
  d <- gets simDisp
  case D.advance dt d of
    Left d' -> do
      modify (\sim -> sim { simDisp = d' })
      return Nothing
    Right (dt1, dt2, name, poke, d') -> do
      modify (\sim -> sim { simDisp = d' })
      return (Just (name, poke, dt1, dt2))

harvestDeps :: Trie () -> State (Sim dt s) [PrName]
harvestDeps tr = do
  ix <- gets simIndex
  let (names, ix') = DI.harvest tr ix
  modify (\sim -> sim { simIndex = ix' })
  return names

rememberDeps :: PrName -> Trie () -> State (Sim dt s) ()
rememberDeps name deps = do
  ix <- gets simIndex
  let ix' = foldl' (flip (DI.insert name)) ix (T.keys deps)
  modify (\sim -> sim { simIndex = ix' })

unschedulePred :: PrName -> State (Sim dt s) ()
unschedulePred name = do
  d <- gets simDisp
  modify (\sim -> sim { simDisp = D.delete name d })

massSchedule :: (Num dt)
             => PrName -> dt -> [((Int,Int),Poke s ())] -> State (Sim dt s) ()
massSchedule name dt entries = do
  d <- gets simDisp
  modify (\sim -> sim { simDisp = D.insert name dt entries d })

shiftModel :: dt -> State (Sim dt s) ()
shiftModel dt = do
  s <- gets simModel
  ani <- gets simAnim
  modify (\sim -> sim { simModel = ani dt s })

applyPoke :: (Num dt) => Poke s () -> State (Sim dt s) [Effect s]
applyPoke poke = do
  s <- gets simModel
  case runPoke s poke of
    Nothing -> return []
    Just (_,s',affectedDeps,effs) -> do
      modify (\sim -> sim { simModel = s' })
      affectedPreds <- harvestDeps affectedDeps
      forM_ affectedPreds $ \name -> do
        unschedulePred name
        Just (AnyPred p) <- gets (H.lookup name . simPreds)
        let (result, deps) = runPredictor p s'
        rememberDeps name deps
        rules <- gets simRules
        case result of
          InExactly dt x -> do
            let pokess = zipWith (\i e -> applyPred i p x e) [0..] rules
            let pokes = concat pokess
            massSchedule name dt pokes
          NotWithin dt -> do
            massSchedule name dt [((0,0),return ())]
          _ -> return ()
      return effs

runMVarState :: MVar s -> State s a -> IO a
runMVarState mv action = modifyMVar mv $ \s -> do
  let (x, s') = runState action s
  return (s', x)

chewTime :: (Num dt, Ord dt) => dt -> State (Sim dt s) [Effect s]
chewTime dt = shiftDispatcher dt >>= \case
  Nothing -> do
    shiftModel dt
    return []
  Just (prName, poke, dt1, dt2) -> do
    shiftModel dt1
    effs1 <- applyPoke poke
    effs2 <- chewTime dt2
    return (effs1 ++ effs2)

setupPortWrite :: Port a -> a -> State (Sim dt s) [Poke s ()]
setupPortWrite port x = do
  rules <- gets simRules
  return $ concatMap (E.applyPortWrite port x) rules

execEffects :: [Effect s] -> IO ()
execEffects effs = forM_ effs $ \case
  FireAndForget io -> io
  _ -> error "async req not implemented"

simulate :: (Num dt, Ord dt)
         => s
         -> A dt s
         -> [Rule dt s]
         -> IO (Interface dt s)
simulate s ani rules = do
  let preds = H.empty
  let disp = D.empty
  let ix = DI.empty
  mv <- newMVar (Sim s ani rules preds disp ix 0)
  return $ Interface
    { simWait = \dt -> do
        effs <- runMVarState mv (chewTime dt)
        execEffects effs
    , simWritePort = \port x -> do
        effs <- runMVarState mv $ do
          pokes <- setupPortWrite port x
          concat <$> mapM applyPoke pokes
        execEffects effs
        return ()
    , simRender = \render -> withMVar mv (return . render . simModel)
    , simKill = takeMVar mv >> return ()
    , simDebug = readMVar mv
    }
