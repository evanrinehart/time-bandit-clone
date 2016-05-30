{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import qualified Data.Vector as V
import Data.Monoid
import System.Exit

import Algorithm
import qualified TimeBandit
import qualified Controller as TimeBandit
import qualified Render as TimeBandit

import Player as P
import GameOver

screenW = 640
screenH = 480

main = iface >>= \x -> playIO mode black 60 x render input advance where
  mode = (InWindow "Testing" (screenW,screenH) (0,0))
  render iface = simImage iface TimeBandit.render
  input e iface = do
    case e of
      EventKey (SpecialKey KeyEsc) _ _ _ -> do
        simKill iface
        exitSuccess
      _ -> do
        simPoke iface (TimeBandit.controller e)
    return iface
  advance dt iface = do
    simWait iface (realToFrac dt)
    --print =<< simDebug iface
    --print =<< ((P.plJoy . TimeBandit.tbPlayer . ungameover . simModel) <$> simDebug iface)
    return iface
  iface = TimeBandit.runTimeBandit
