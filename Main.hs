{-# LANGUAGE LambdaCase #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import Graphics.Gloss.Juicy
import qualified Data.Vector as V
import Data.Monoid
import System.Exit

import Algorithm
import qualified TimeBandit
import qualified Controller as TimeBandit
import qualified Render as TimeBandit

import Player as P
import GameOver
import Level
import Model
import TileGrid
import Port

screenW = 640
screenH = 480

render iface = simRender iface TimeBandit.render

input pA pB e iface = do
  case e of
    EventKey (SpecialKey KeyEsc) _ _ _ -> do
      simKill iface
      exitSuccess
    _ -> do
      (TimeBandit.controller pA pB iface e)
      --return ()
  return iface

advance dt iface = do
  simWait iface (realToFrac dt)
  --print =<< simModel <$> simDebug iface
  print =<< simDebug iface
  --print =<< ((lvlMissiles . currentLevel . tbLvls . ungameover . simModel) <$> simDebug iface)
  return iface

main = do
  let mode = (InWindow "Testing" (screenW,screenH) (0,0))
  pA <- newPort
  pB <- newPort
  iface <- TimeBandit.runTimeBandit pA pB exampleGrid
  playIO mode black 60 iface render (input pA pB) advance
