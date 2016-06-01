{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Plan where

import Prelude hiding (read)
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except

import Data.ByteString
import Viewing
import Path

-- planning monad reads from a state, using paths, tracking which paths
-- it used. it can also abort early without producing a result.

-- runPlan, fail, view, viewMaybe

newtype Plan s a =
  Plan { unPlan :: ReaderT s (ExceptT String (Writer [ByteString])) a }
  deriving Functor

instance Monad (Plan s) where
  return x = Plan (return x)
  (Plan act) >>= f = Plan (act >>= \x -> unPlan (f x))
  fail = abort

instance Applicative (Plan s) where
  pure = return
  (<*>) = ap

instance Viewing (Plan s) where
  type ViewingSubject (Plan s) = s
  viewMaybe path = do
    addDep (pathRep path)
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

addDep :: ByteString -> Plan s ()
addDep x = Plan (tell [x])

withState :: (s -> a) -> Plan s a
withState f = Plan (asks f)

runPlan :: s -> Plan s a -> (Maybe a, [ByteString])
runPlan s (Plan act) =
  let (result, reps) = runWriter (runExceptT (runReaderT act s)) in
  case result of
    Left msg -> (Nothing, reps)
    Right x -> (Just x, reps)
