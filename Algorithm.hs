{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Algorithm where

import Prelude hiding ((.),id)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.List (intersperse, foldl')
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Control.Concurrent
import Data.Fixed
import Control.Monad (forM_)

import Dispatcher as D
import Plan
import Poke
import Animation as A
import Path
import Viewing
import DepIndex as DI
import Workers as W

import Control.Monad (when)
import Control.Category

{-
  0. eval all rules, update index and a set of inactive / waiting actions
  1. wait for next action, user input, or async result
  2. take all actions happening now, and sort them by priority. execute the
     poke actions in order to get an abort or list of paths modified for each
     one.
  3. search for a set of rules that were "interrupted" by each modification
  4. cancel all action of interrupted rules, and remove them from the index.
  5. go to step 0, except eval only the interrupted rules
-}

data Rule dt s = Rule String (Plan s (dt, Poke s ()))
data Handler s = forall a . Handler (IO a) [a -> Poke s ()]
data RuleStatus dt = Inactive Int String String | Staged Int String dt

data Sim dt s = Sim
  { simModel :: s
  , simAnimation :: A dt s
  , simIndex :: DepIndex
  , simRules :: IntMap (Rule dt s)
  , simDisp :: Disp dt (Int, Poke s ())
  , simHandlers :: [Handler s]
  } 

instance (Show dt, Show s) => Show (Sim dt s) where
  show (Sim x _ ix rules disp _) = "Sim {" ++ (concat . intersperse "," $
    [show x
    ,show ix
    ,show (map (\(Rule name _) -> name) $ IM.elems rules)
    ,show (fmap fst disp)
    ] ) ++ "}"

newSimulation :: (Num dt, Ord dt) => s -> A dt s -> [Rule dt s] -> [Handler s] -> Sim dt s
newSimulation s adv rules handlers = Sim s adv ix rs disp handlers where
  numberedRules = zip [0..] rules
  (actions0, ix) = evaluateRules s numberedRules
  rs = IM.fromList numberedRules
  disp = D.Disp 0 (map (\(dt,i,act) -> (dt,(i,act))) actions0)

evaluateRules :: s -> [(Int, Rule dt s)] -> ([(dt, Int, Poke s ())], DepIndex)
evaluateRules s xs = fmap DI.fromList (go [] [] xs) where
  go o ix [] = (o, ix)
  go o ix ((i,Rule name pl):xs) = case runPlan s pl of
    Left (msg, reps) -> go o (map (,i) reps ++ ix) xs
    Right ((dt,poke), reps) -> go ((dt,i,poke):o) (map (,i) reps ++ ix) xs

animate :: (Num dt, Ord dt) => dt -> Sim dt s -> Either (Sim dt s) (Sim dt s, dt, Int, Poke s ())
animate dt sim@(Sim s adv ix rs disp hs) = case D.advance dt disp of
  (Nothing, disp') -> Left (Sim (adv dt s) adv ix rs disp' hs)
  (Just ((i,poke), dt'), disp') -> Right (sim', dt - dt', i, poke) where
    sim' = Sim (adv dt' s) adv (DI.remove i ix) rs disp' hs

applyPoke :: (Num dt, Ord dt) => Poke s () -> Sim dt s -> (Sim dt s, [IO ()], [Request s])
applyPoke poke sim@(Sim s adv ix rs disp hs) = case runPoke s poke of
  Left msg -> (sim, [], [])
  Right (_,s',reps,ios,reqs) -> (Sim s' adv ix''' rs disp'' hs, ios, reqs) where
    affectedRids = foldl' IS.union IS.empty (map (DI.match ix) reps)
    disp' = D.deleteBy ((`IS.member` affectedRids).fst) disp
    ix' = DI.removeMany affectedRids ix
    getRuleWithNumber i = (i,r) where
      Just r = IM.lookup i rs
    numberedRules = map getRuleWithNumber (IS.toList affectedRids)
    (newTasks, ix'') = evaluateRules s' numberedRules
    ix''' = ix' `DI.union` ix''
    disp'' = foldl' (\d (dt,i,poke) -> D.insert dt (i,poke) d) disp' newTasks

evalRule :: (Num dt, Ord dt) => Int -> Sim dt s -> Sim dt s
evalRule i (Sim s adv ix rs disp hs) = case IM.lookup i rs of
  Nothing -> error ("rule " ++ show i ++ " not found")
  Just (Rule name pl) -> case runPlan s pl of
    Left (msg, reps) -> Sim s adv ix' rs disp hs where
      ix' = ix `DI.union` (DI.fromList (map (,i) reps))
    Right ((dt,poke), reps) -> Sim s adv ix' rs disp' hs where
      ix' = ix `DI.union` (DI.fromList (map (,i) reps))
      disp' = D.insert dt (i,poke) disp

data Interface dt s = Interface
   { simWait :: dt -> IO ()
   , simImage :: forall a . (s -> a) -> IO a
   , simPoke :: Poke s () -> IO ()
   , simKill :: IO ()
   , simDebug :: IO (Sim dt s)
   }

instance Show (Interface dt s) where
  show _ = "<Interface>"

test :: (Num dt, RealFrac dt, Show s) => Sim dt s -> IO ()
test sim = go us dt sim where
  million = 1000000
  dt = 0.1
  us = ceiling (dt * million)
  go us dt sim = do
    print (simModel sim)
    threadDelay us
    case animate dt sim of
      Left sim' -> test sim'
      Right (sim', dt', ruleId, poke) -> do
        let (sim'', ios, reqs) = applyPoke poke sim'
        sequence_ ios
        let sim''' = evalRule ruleId sim''
        --asyncs
        go (ceiling (dt' * million)) dt' sim'''

spawnRequests :: Workers -> MVar (Sim dt s) -> [Request s] -> IO ()
spawnRequests workers mv reqs = do
  return ()

handleInput :: (Num dt, Ord dt)
            => Workers
            -> MVar (Sim dt s)
            -> Poke s ()
            -> IO ()
handleInput workers mv poke = modifyMVar_ mv $ \sim -> do
  let (sim', ios, reqs) = applyPoke poke sim
  sequence_ ios
  spawnRequests workers mv reqs
  return sim'

run :: (Num dt, Ord dt) => Sim dt s -> IO (Interface dt s)
run sim = iface where
  iface = do
    mv <- newMVar sim 
    workers <- newWorkers
    forM_ (simHandlers sim) $ \(Handler io hs) -> do
      forM_ hs $ \h -> do
        spawnInputHandler "(untitled)" workers io (handleInput workers mv . h)
    withMVar workers print
    return $ Interface
      (advance mv workers)
      (image mv)
      (input mv workers)
      (kill mv workers)
      (debug mv)
  advance mv workers dt = modifyMVar_ mv (go dt) where
    go dt sim = case animate dt sim of
      Left sim' -> return sim'
      Right (sim', dt', ruleId, poke) -> do
        let (sim'', ios, reqs) = applyPoke poke sim'
        let sim''' = evalRule ruleId sim''
        sequence_ ios
        spawnRequests workers mv reqs
        go dt' sim'''
  image mv f = withMVar mv (return . f . simModel)
  input mv workers poke = modifyMVar_ mv $ \sim -> do
    let (sim', ios, reqs) = applyPoke poke sim
    sequence_ ios
    spawnRequests workers mv reqs
    return sim'
  kill mv workers = do
    takeMVar mv
    clearWorkers workers
  debug mv = withMVar mv return

planToLater_ :: Poke s () -> dt -> Plan s (dt, Poke s ())
planToLater_ poke dt = return (dt, poke)

planToNow_ :: Num dt => Poke s () -> Plan s (dt, Poke s ())
planToNow_ poke = return (0, poke)

require :: Path s Bool -> Plan s ()
require p = do
  b <- view p
  when b (fail "requirement failed")

requireThat :: (a -> Bool) -> Path s a -> Plan s ()
requireThat f p = do
  x <- view p
  when (f x) (fail "requirement failed")

planToWait :: dt -> Plan s (dt, Poke s ())
planToWait dt = return (dt, return ())

planTo_ :: dt -> Poke s () -> Plan s (dt, Poke s ())
planTo_ dt poke = return (dt, poke)

