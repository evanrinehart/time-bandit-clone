{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Viewing where

import Path

class Monad f => Viewing f where
  type ViewingSubject f
  viewMaybe :: Path (ViewingSubject f) a -> f (Maybe a)
  view :: Path (ViewingSubject f) a -> f a
  view path = viewMaybe path >>= \case
    Nothing -> fail "unable to get data at path"
    Just x -> return x
