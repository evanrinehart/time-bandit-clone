{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Algorithm where

import Prelude hiding ((.),id)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.List (intersperse, foldl')
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Control.Concurrent
import Data.Fixed

import Dispatcher as D
import Plan
import Poke
import Animation as A
import Path
import Viewing
import DepIndex as DI

import Pause
import Clock
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

data RuleStatus dt = Inactive Int String String | Staged Int String dt

data Sim dt s = Sim
  { simAnimation :: A dt s
  , simIndex :: DepIndex
  , simRules :: IntMap (Rule dt s)
  , simDisp :: Disp dt (Int, Poke s ())
  } 

instance (Show dt, Show s) => Show (Sim dt s) where
  show (Sim x ix rules disp) = "Sim {" ++ (concat . intersperse "," $
    [show x
    ,show ix
    ,show (map (\(Rule name _) -> name) $ IM.elems rules)
    ,show (fmap fst disp)
    ] ) ++ "}"

newSimulation :: (Num dt, Ord dt) => A dt s -> [Rule dt s] -> Sim dt s
newSimulation anim rules = Sim anim ix rs disp where
  numberedRules = zip [0..] rules
  (actions0, ix) = evaluateRules (sample anim) numberedRules
  rs = IM.fromList numberedRules
  disp = D.Disp 0 (map (\(dt,i,act) -> (dt,(i,act))) actions0)

evaluateRules :: s -> [(Int, Rule dt s)] -> ([(dt, Int, Poke s ())], DepIndex)
evaluateRules s xs = fmap DI.fromList (go [] [] xs) where
  go o ix [] = (o, ix)
  go o ix ((i,Rule name pl):xs) = case runPlan s pl of
    Left (msg, reps) -> go o (map (,i) reps ++ ix) xs
    Right ((dt,poke), reps) -> go ((dt,i,poke):o) (map (,i) reps ++ ix) xs

animate :: (Num dt, Ord dt) => dt -> Sim dt s -> Either (Sim dt s) (Sim dt s, dt, Int, Poke s ())
animate dt sim@(Sim a ix rs disp) = case D.advance dt disp of
  (Nothing, disp') -> Left sim' where
    sim' = sim
      { simAnimation = A.advance dt a
      , simDisp = disp'
      }
  (Just ((i,poke), dt'), disp') -> Right (sim', dt - dt', i, poke) where
    sim' = sim
      { simAnimation = A.advance dt' a
      , simIndex = DI.remove i ix
      , simDisp = disp' }

-- do this
applyPoke :: (Num dt, Ord dt) => Poke s () -> Sim dt s -> (Sim dt s, [IO ()], [Request s])
applyPoke poke sim@(Sim a ix rs disp) = case runPoke (sample a) poke of
  Left msg -> (sim, [], [])
  Right (_,s',reps,ios,reqs) -> (Sim a' ix''' rs disp'', ios, reqs) where
    a' = A.modify (const s') a
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
evalRule i (Sim a ix rs disp) = case IM.lookup i rs of
  Nothing -> error ("rule " ++ show i ++ " not found")
  Just (Rule name pl) -> case runPlan (sample a) pl of
    Left (msg, reps) -> Sim a ix' rs disp where
      ix' = ix `DI.union` (DI.fromList (map (,i) reps))
    Right ((dt,poke), reps) -> Sim a ix' rs disp' where
      ix' = ix `DI.union` (DI.fromList (map (,i) reps))
      disp' = D.insert dt (i,poke) disp
      
example :: Num dt => Rule dt (Int, (Char, Bool))
example = Rule "example" $ do
  c <- view (second >>> first)
  when (c == 'x') (fail "miserably")
  return (0, return ())

example2 :: Rule Delta (Clock Delta ())
example2 = Rule "example2" $ do
  t <- view time
  r <- view rate
  let hmm = -t / r
  if hmm < 0 then fail "time out" else return (hmm, return ())

exsim :: Sim Pico (Clock Pico (), Clock Pico ())
exsim = newSimulation 
  (A (Clock 0 1 (), Clock 1 (-1) ()) (fuse (clock blank) (clock blank)))
  []
  

test :: (Num dt, RealFrac dt, Show s) => Sim dt s -> IO ()
test sim = go us dt sim where
  dt = 0.1
  us = ceiling (dt * 1000000)
  go us dt sim = do
    print (simAnimation sim)
    threadDelay us
    case animate dt sim of
      Left sim' -> test sim'
      Right (sim', dt', ruleId, poke) -> do
        let (sim'', ios, reqs) = applyPoke poke sim'
        sequence_ ios
        --nothing
        go (ceiling (dt' * 1000000)) dt' sim''
