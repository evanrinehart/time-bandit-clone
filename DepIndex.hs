module DepIndex where

import Data.List hiding (union)
import Data.ByteString (ByteString)
import Data.Trie (Trie)
import Data.IntSet (IntSet)
import qualified Data.Trie as T
import qualified Data.Trie.Convenience as T
import qualified Data.IntSet as S

newtype DepIndex = DepIndex { getTrie :: Trie IntSet } deriving (Show)

onTrie :: (Trie IntSet -> Trie IntSet) -> DepIndex -> DepIndex
onTrie f (DepIndex tr) = DepIndex (f tr)

empty :: DepIndex
empty = DepIndex (T.empty)

singleton :: ByteString -> Int -> DepIndex
singleton k i = DepIndex (T.singleton k (S.singleton i))

fromList :: [(ByteString, Int)] -> DepIndex
fromList = foldl' f empty . map (\(x,y) -> (x,S.singleton y)) where
  f tr (k,xs) = append k xs tr

append :: ByteString -> IntSet -> DepIndex -> DepIndex
append k xs = onTrie (T.alterBy f k xs) where
  f _ xs Nothing = Just xs
  f _ xs (Just ys) = Just (S.union xs ys)

match :: DepIndex -> ByteString -> IntSet
match tr k = (matchAlg k . getTrie) tr

union :: DepIndex -> DepIndex -> DepIndex
union tr1 tr2 = DepIndex $ T.unionWith (S.union) (getTrie tr1) (getTrie tr2)

remove :: Int -> DepIndex -> DepIndex
remove i = onTrie (fmap (S.delete i))

removeMany :: IntSet -> DepIndex -> DepIndex
removeMany ints = onTrie (T.filterMap g) where
  g ints' = let ints'' = S.difference ints' ints in if S.null ints''
    then Nothing
    else Just ints''

matchAlg :: ByteString -> Trie IntSet -> IntSet
matchAlg k tr = S.union leading under where
  leading = foldl' S.union S.empty (map (\(_,xs,_) -> xs) $ T.matches tr k)
  under = T.lookupBy (\_ -> foldl' S.union S.empty) k tr
