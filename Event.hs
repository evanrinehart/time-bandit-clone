{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Event where

import Data.HashMap.Strict as H
import System.Mem.StableName
import Unsafe.Coerce
import Poke
import Prediction
import Port
import Plan

data Event :: * -> * -> * -> * where
  ENever :: Event dt s a
  EPrediction :: Pred dt s a -> Event dt s a
  EInterrupt :: Port a -> Event dt s a
  EGate :: Plan s Bool -> Event dt s a -> Event dt s a
  EMap :: (a -> b) -> Event dt s a -> Event dt s b
  ECat :: Event dt s a -> Event dt s a -> Event dt s a

instance Functor (Event dt s) where
  fmap = EMap

instance Monoid (Event dt s a) where
  mempty = ENever
  mappend = ECat
    
fromEvent :: Event dt s a -> PrSet dt s -> IO (PrSet dt s)
fromEvent e0 hm = case e0 of
  EPrediction pred -> do
    name <- PrName <$> (makeStableName $! pred)
    if H.member name hm
      then return hm
      else return (H.insert name (AnyPred pred) hm)
  ECat e1 e2 -> fromEvent e1 hm >>= fromEvent e2
  EMap f e -> fromEvent e hm
  EGate pl e -> fromEvent e hm
  _ -> return hm

applyPred :: forall a dt s . a
          -> Pred dt s a
          -> s
          -> Event dt s (Poke s ())
          -> IO [Poke s ()]
applyPred x pred0 s e0 = go id e0 [] where
  go :: forall b . (b -> Poke s ())
     -> Event dt s b
     -> [Poke s ()]
     -> IO [Poke s ()]
  go f e outs = case e of
    EPrediction pred1 -> predCoerce pred0 pred1 x >>= \case
      Nothing -> return outs
      Just x' -> return (f x' : outs)
    ECat e1 e2 -> go f e1 outs >>= go f e2
    EMap g e' -> go (f . g) e' outs
    EGate pl e' -> case runPlan pl s of
      (Nothing, _) -> return outs
      (Just False, _) -> return outs
      _ -> go f e' outs
    _ -> return outs

applyPred1 :: forall a dt s . a
           -> Pred dt s a
           -> s
           -> Event dt s (Poke s ())
           -> IO (Maybe (Poke s ()))
applyPred1 x pred0 s e0 = go id e0 where
  go :: forall b . (b -> Poke s ())
     -> Event dt s b
     -> IO (Maybe (Poke s ()))
  go f e = case e of
    EPrediction pred1 -> predCoerce pred0 pred1 x >>= \case
      Nothing -> return Nothing
      Just x' -> return (Just (f x'))
    ECat e1 e2 -> do
      go f e1 >>= \case
        Nothing -> go f e2
        Just x -> return (Just x)
    EMap g e' -> go (f . g) e'
    EGate pl e' -> case runPlan pl s of
      (Nothing, _) -> return Nothing
      (Just False, _) -> return Nothing
      _ -> go f e'
    _ -> return Nothing
