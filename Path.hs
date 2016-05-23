{-# LANGUAGE FlexibleInstances #-}
module Path where

import Prelude hiding ((.), id)
import Control.Category
import Data.Monoid
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Bits
import Data.Word
import Data.Map as M
import Control.Monad ((<=<))

type Edit s a = (a -> a) -> s -> s
data Path s a = Path ByteString (s -> Maybe a) (Edit s a)

w8 :: Word8 -> ByteString
w8 x = BS.singleton x

byte :: Int -> Word64 -> Word8
byte i = fromIntegral . (.&. 255) . (`shiftR` (8*i))

w64 :: Word64 -> ByteString
w64 x = BS.pack
  [ byte 7 x
  , byte 6 x
  , byte 5 x
  , byte 4 x
  , byte 3 x
  , byte 2 x
  , byte 1 x
  , byte 0 x ]

pathGet :: Path s a -> s -> Maybe a
pathGet (Path _ g _) x = g x

pathUpdate :: Path s a -> (a -> a) -> s -> s
pathUpdate (Path _ _ e) f x = e f x

pathRep :: Path s a -> ByteString
pathRep (Path bs _ _) = bs

unit :: Path () ()
unit = Path (w8 0) Just (const id)

first :: Path (a, b) a
first = Path (w8 0) (Just . fst) (\f (x,y) -> (f x, y))

second :: Path (a, b) b
second = Path (w8 1) (Just . snd) (\f (x,y) -> (x, f y))

key :: (Enum k, Ord k) => k -> Path (Map k a) a
key k = Path (w64 . fromIntegral . fromEnum $ k) (M.lookup k) (\f -> M.adjust f k)

left :: Path (Either a b) a
left = Path (w8 0) g p where
  g (Left x) = Just x
  g _ = Nothing
  p f (Left x) = Left (f x)
  p f other = other

right :: Path (Either a b) b
right = Path (w8 1) g p where
  g (Right x) = Just x
  g _ = Nothing
  p f (Right x) = Right (f x)
  p f other = other

instance Category Path where
  id = Path mempty Just (const id)
  (Path p1 g1 e1) . (Path p2 g2 e2) = Path (p1 <> p2) (g1 <=< g2) (e2 . e1)

class Functor f => Body f where
  getBody :: f a -> a

body :: Body f => Path (f a) a
body = Path (w8 0) (Just . getBody) fmap

