{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Random (evalRand, newStdGen)
import Graphics.Gloss.Interface.IO.Game
import System.Exit

import Snakes

main :: IO ()
main = do
  u <- spawnPlayer "You" emptyUniverse
  initialWorld <- atomically $ newTVar u
  spawnBot "Bot 1" simpleBot initialWorld
  spawnBot "Bot 2" simpleBot initialWorld
  spawnBot "Bot 3" simpleBot initialWorld

  playIO display bgColor fps initialWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "The Game of Snakes" (w, h) (100, 100)
    bgColor = black
    fps     = 60

    renderWorld w = do
      u <- readTVarIO w
      return (renderUniverse u)

    updateWorld dt w = do
      g <- newStdGen
      atomically $ modifyTVar w (flip evalRand g . updateUniverse dt)
      return w

    handleWorld (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess -- exit on ESC
    handleWorld (EventMotion mouse) w = atomically $ do
      u <- readTVar w
      writeTVar w (handleSnakeAction "You" (SnakeRedirect mouse) u)
      return w
    handleWorld _ w = return w

    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)

-- | Add a bot to the 'Universe'.
spawnBot :: PlayerName -> Bot -> TVar Universe -> IO ()
spawnBot name bot w = do
  g <- newStdGen
  atomically $ modifyTVar w (flip evalRand g . spawnPlayer name)
  forkIO $ forever $ do
    threadDelay 1000
    atomically $ do
      u <- readTVar w
      case bot name u of
        Just action -> writeTVar w (handleSnakeAction name action u)
        Nothing -> return ()
  return ()
