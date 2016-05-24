{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Poke where

import Prelude hiding (read)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Exception

import Data.ByteString
import Viewing
import Path

-- reaction monad can modify the state arbitrarily, execute IO actions,
-- and bailout early, which "undoes" any effects. it can also read the
-- state.

newtype Poke s a =
  Poke { unPoke :: InnerMonad s a }
  deriving Functor

type InnerMonad s = StateT s (WriterT (Output s) (Either String))
type Output s = ([ByteString], [IO ()], [Request s])
data Request s = forall a . Request (IO a) (Callback s a) (Maybe Int)
type Callback s a = forall e . Exception e => Either e a -> Poke s ()

instance Monad (Poke s) where
  return x = Poke (return x)
  (Poke act) >>= f = Poke (act >>= \x -> unPoke (f x))
  fail = abort

instance Applicative (Poke s) where
  pure = return
  (<*>) = ap

-- runPoke, fail, view, viewMaybe, update, execIO, doRequest, doRequestWithTimeout

instance Viewing (Poke s) where
  type ViewingSubject (Poke s) = s
  viewMaybe path = do
    s <- Poke get
    return (pathGet path s)

instance Show (Poke s a) where
  show _ = "<Poke>"

update :: Path s a -> (a -> a) -> Poke s ()
update path f = do
  addDep (pathRep path)
  s <- Poke get
  let s' = pathUpdate path f s
  Poke (put s')

set :: Path s a -> a -> Poke s ()
set path x = update path (const x)

execIO :: IO () -> Poke s ()
execIO io = Poke (tell ([], [io], []))

doRequest :: IO a -> Callback s a -> Poke s ()
doRequest io cb = Poke (tell ([],[],[Request io cb Nothing]))

doRequestWithTimeout :: Int -> IO a -> Callback s a -> Poke s ()
doRequestWithTimeout ms io cb = Poke (tell ([],[],[Request io cb (Just ms)]))

doRequestIgnoreFailures :: IO a -> (a -> Poke s ()) -> Poke s ()
doRequestIgnoreFailures io cb = Poke (tell ([],[],[req])) where
  req = Request io (ignoreFailures cb) Nothing

doRequestWithTimeoutIgnoreFailures :: Int -> IO a -> (a -> Poke s ()) -> Poke s ()
doRequestWithTimeoutIgnoreFailures ms io cb = Poke (tell ([],[],[req])) where
  req = Request io (ignoreFailures cb) (Just ms)

ignoreFailures :: (a -> Poke s ()) -> (forall e . Exception e => Either e a -> Poke s ())
ignoreFailures cb (Right x) = cb x
ignoreFailures cb _ = return ()

abort :: String -> Poke s a
abort msg = Poke (throwError msg)

addDep :: ByteString -> Poke s ()
addDep x = Poke (tell ([x],[],[]))

runPoke :: s -> Poke s a -> Either String (a, s, [ByteString], [IO ()], [Request s])
runPoke s (Poke act) = case runWriterT (runStateT act s) of
  Left msg -> Left msg
  Right ((x, s'), (reps,ios,reqs)) -> Right (x, s', reps, ios, reqs)
