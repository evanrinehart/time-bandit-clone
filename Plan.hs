{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Plan where

import Prelude hiding (read)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Applicative as A
import Data.Monoid
import Data.Trie as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Viewing
import Path

-- planning monad reads from a state, using paths, tracking which paths
-- it used. it can also abort early without producing a result.

newtype Plan s a =
  Plan { unPlan :: ReaderT s (ExceptT String (Writer PlanDep)) a }
  deriving Functor

type PlanDep = Endo (Trie ())

instance Monad (Plan s) where
  return x = Plan (return x)
  (Plan act) >>= f = Plan (act >>= \x -> unPlan (f x))
  fail = abort

instance Applicative (Plan s) where
  pure = return
  (<*>) = ap

instance Alternative (Plan s) where
  empty = Plan A.empty
  (Plan act1) <|> (Plan act2) = Plan (act1 <|> act2)

instance Viewing (Plan s) where
  type ViewingSubject (Plan s) = s
  viewMaybe path = do
    addModelDep (pathRep path)
    withState (pathGet path)

instance Show (Plan s a) where
  show _ = "<Plan>"

notApplicable :: Plan s a
notApplicable = fail "not applicable"

notApplicableIf :: Bool -> Plan s ()
notApplicableIf b = if b then notApplicable else return ()

applicableOnlyIf :: Bool -> Plan s ()
applicableOnlyIf b = if b then return () else notApplicable

required :: Maybe a -> Plan s a
required = maybe (fail "view failed") return 

abort :: String -> Plan s a
abort msg = Plan (throwError msg)

addModelDep :: ByteString -> Plan s ()
addModelDep bs = Plan . tell . Endo . T.insert bs $ ()

withState :: (s -> a) -> Plan s a
withState f = Plan (asks f)

runPlan :: Plan s a -> s -> (Maybe a, Trie ())
runPlan (Plan act) s =
  let (result, mkdeps) = runWriter (runExceptT (runReaderT act s)) in
  let deps = appEndo mkdeps T.empty in
  case result of
    Left _ -> (Nothing, deps)
    Right x -> (Just x, deps)
