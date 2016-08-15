{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Snakes

main :: IO ()
main = do
  u <- randomUniverse cfg
  initialWorld <- atomically $ newTVar (addPlayer "You" u cfg)
  addBot "Bot 1" simpleBot  initialWorld cfg
  addBot "Bot 2" phantomBot initialWorld cfg
  addBot "Bot 3" bonusBot   initialWorld cfg
  addBot "Bot 4" simpleBot  initialWorld cfg
  addBot "Bot 5" phantomBot initialWorld cfg

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

addBot :: PlayerName -> Bot -> TVar Universe -> GameConfig -> IO ()
addBot name bot w cfg = do
  atomically $ do
    u <- readTVar w
    writeTVar w (addPlayer name u cfg)
  forkIO $ forever $ do
    threadDelay 1000
    atomically $ do
      u <- readTVar w
      case bot u of
        Just action -> writeTVar w (handlePlayerAction name action u cfg)
        Nothing -> return ()
  return ()
