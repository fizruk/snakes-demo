{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent.STM
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Snakes

main :: IO ()
main = do
  u <- randomUniverse cfg
  initialWorld <- atomically $ newTVar (addPlayer "You" u cfg)
  playIO display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (w, h) (100, 100)
    bgColor = black
    fps     = 60

    renderWorld w = do
      u <- readTVarIO w
      return (renderUniverse u cfg)

    updateWorld dt w = atomically $ do
      u <- readTVar w
      writeTVar w (updateUniverse dt u cfg)
      return w

    handleWorld (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess -- exit on ESC
    handleWorld (EventMotion mouse) w = atomically $ do
      u <- readTVar w
      writeTVar w (handlePlayerAction "You" (RedirectSnake mouse) u cfg)
      return w
    handleWorld _ w = return w

    cfg@GameConfig{..} = defaultGameConfig
    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)
