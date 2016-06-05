{-# LANGUAGE BangPatterns #-}
module DepIndex2 where

import Data.Trie as T hiding (insert)
import Data.ByteString as BS
import Data.List as L hiding (insert)

-- The Dep Index is a relation between predictor objects and locations in
-- the model, which are represented as bytestrings. A prediction is invalidated
-- if a location is modified that has a bytestring that is a prefix or has as
-- prefix any related bytestring to the prediction. The main operation here
-- is harvest, which takes a set of bytestrings (a trie) and returns a list
-- of predictor IDs affected, while removing the relationships from the index.

type DepIx2 a b = [(a, b)]

empty :: DepIx2 a b
empty = []

insert :: a -> b -> DepIx2 a b -> DepIx2 a b
insert !x !y db = (x,y):db

harvest :: Eq a => Trie b -> DepIx2 a ByteString -> ([a], DepIx2 a ByteString)
harvest tr ix = let (as,ix') = go [] [] ix in (nub as, ix') where
  go as ix' [] = (as,ix')
  go as ix' (entry@(a,bs):more) =
    if not (T.null (submap bs tr)) || not (L.null (match tr bs))
      then go (a:as) ix' more
      else go as (entry:ix') more 
