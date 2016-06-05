{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
module Poke where

import Prelude hiding (read)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Exception
import Data.Trie as T
import Data.Monoid

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
type Output s = (Endo (Trie ()), [Effect s])
type Callback s a = forall e . Exception e => Either e a -> Poke s ()

data Effect s where
  FireAndForget :: IO () -> Effect s
  RequestWithCallback :: IO a -> Maybe Int -> Callback s a -> Effect s

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
  let !s' = pathUpdate path f s
  Poke (put s')

updateMaybe :: Path s a -> (a -> Maybe a) -> Poke s ()
updateMaybe path f = do
  s <- Poke get
  case pathGet path s of
    Nothing -> return ()
    Just x -> case f x of
      Nothing -> return ()
      Just x' -> do
        let !s' = pathUpdate path (const x') s
        addDep (pathRep path)
        Poke (put s')

set :: Path s a -> a -> Poke s ()
set path x = update path (const x)

execIO :: IO () -> Poke s ()
execIO io = Poke (tell (mempty, [FireAndForget io]))

doRequest :: IO a -> Callback s a -> Poke s ()
doRequest io cb = Poke (tell (mempty,[RequestWithCallback io Nothing cb]))

doRequestWithTimeout :: Int -> IO a -> Callback s a -> Poke s ()
doRequestWithTimeout ms io cb =
  Poke (tell (mempty,[RequestWithCallback io (Just ms) cb]))

doRequestIgnoreFailures :: IO a -> (a -> Poke s ()) -> Poke s ()
doRequestIgnoreFailures io cb = Poke (tell (mempty,[req])) where
  req = RequestWithCallback io Nothing (ignoreFailures cb) 

doRequestWithTimeoutIgnoreFailures ::
  Int -> IO a -> (a -> Poke s ()) -> Poke s ()
doRequestWithTimeoutIgnoreFailures ms io cb = Poke (tell (mempty,[req])) where
  req = RequestWithCallback io (Just ms) (ignoreFailures cb)

ignoreFailures ::
  (a -> Poke s ()) -> (forall e . Exception e => Either e a -> Poke s ())
ignoreFailures cb (Right x) = cb x
ignoreFailures cb _ = return ()

abort :: String -> Poke s a
abort msg = Poke (throwError msg)

addDep :: ByteString -> Poke s ()
addDep bs = Poke (tell (Endo (T.insert bs ()),[]))

-- introduce an artificial modification
touch :: Path s a -> Poke s ()
touch p = do
  let !bs = pathRep p
  addDep bs
  return ()

runPoke :: s
        -> Poke s a
        -> Either String (a, s, Trie (), [Effect s])
runPoke s (Poke act) = case runWriterT (runStateT act s) of
  Left msg -> Left msg
  Right ((!x, !s'), (mkdeps,effs)) ->
    Right (x, s', appEndo mkdeps T.empty, effs)
