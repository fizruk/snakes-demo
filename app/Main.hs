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
  u <- randomUniverse
  initialWorld <- atomically $ newTVar (addPlayer "You" u)
  addBot "Bot 1" simpleBot initialWorld

  playIO display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (w, h) (100, 100)
    bgColor = black
    fps     = 60

    renderWorld w = do
      u <- readTVarIO w
      return (renderUniverse u)

    updateWorld dt w = atomically $ do
      u <- readTVar w
      writeTVar w (updateUniverse dt u)
      return w

    handleWorld (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess -- exit on ESC
    handleWorld (EventMotion mouse) w = atomically $ do
      u <- readTVar w
      writeTVar w (handleSnakeAction "You" (SnakeRedirect mouse) u)
      return w
    handleWorld _ w = return w

    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)

addBot :: PlayerName -> Bot -> TVar Universe -> IO ()
addBot name bot w = do
  atomically $ do
    u <- readTVar w
    writeTVar w (addPlayer name u)
  forkIO $ forever $ do
    threadDelay 1000
    atomically $ do
      u <- readTVar w
      case bot u of
        Just action -> writeTVar w (handleSnakeAction name action u)
        Nothing -> return ()
  return ()
