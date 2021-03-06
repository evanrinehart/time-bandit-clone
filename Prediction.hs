{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Prediction where

import Data.Map
import System.Mem.Weak
import System.Mem.StableName
import Control.Concurrent.STM
import Data.ByteString as BS
import Data.Trie as T
import Data.HashMap.Strict as H
import Data.Hashable
import Unsafe.Coerce
import System.IO.Unsafe

import Plan
import Poke

data Prediction dt a =
  WontEver |
  InExactly dt a |
  NotWithin dt
    deriving (Show,Functor)

type Pred dt s a = Plan s (Prediction dt a)
data PrName = forall a . PrName (StableName a)
data AnyPred dt s = forall a . AnyPred { unAnyPred :: Pred dt s a }
type PrSet dt s = HashMap PrName (AnyPred dt s)

instance Eq PrName where
   PrName n1 == PrName n2 = eqStableName n1 n2

instance Hashable PrName where
   hashWithSalt salt (PrName n) = hashWithSalt salt (hashStableName n)

instance Show PrName where
  show _ = "<Pred>"

dummyPred :: Pred Double s Char
dummyPred = do
  return (InExactly 3.14 'z')

-- run a predictor on a state.
runPredictor :: Pred dt s a -> s -> (Prediction dt a, Trie ())
runPredictor pred s = (result, deps) where
  (mresult, deps) = runPlan pred s
  result = case mresult of
    Nothing -> WontEver
    Just r -> r

-- if they have the same stable name, they have the same result type
-- so you can safely do a type cast.
predCoerce :: Pred dt s a -> Pred dt s b -> a -> Maybe b
predCoerce !p1 !p2 x = unsafePerformIO $ do
  n1 <- makeStableName p1
  n2 <- makeStableName p2
  if eqStableName n1 n2
    then return (Just (unsafeCoerce x))
    else return Nothing
