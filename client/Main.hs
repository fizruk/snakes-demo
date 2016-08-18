{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)

import Graphics.Gloss.Interface.IO.Game
import Network.WebSockets
import System.Exit (exitSuccess)

import Snakes

-- | Game state on client.
data GameState = GameState
  { gameUniverse    :: TVar Universe    -- ^ The current state of the universe.
  , gameConnection  :: Connection       -- ^ Websocket 'Connection' with the server.
  }

-- | Accelerate player's 'Dot'.
-- This only sends a command to server.
redirectSnake :: Point -> GameState -> IO GameState
redirectSnake mouse g@GameState{..} = do
  -- we fork to avoid interface freezing
  _ <- forkIO $ sendBinaryData gameConnection (SnakeRedirect mouse)
  return g

-- | Handle user input.
handleGame :: Event -> GameState -> IO GameState
handleGame (EventKey (SpecialKey KeyEsc)   Down _ _) = const exitSuccess    -- exit on ESC
handleGame (EventMotion mouse) = redirectSnake mouse
handleGame _ = return

-- | Handle 'Universe' updates coming from server.
handleUpdates :: GameState -> IO ()
handleUpdates GameState{..} = forever $ do
  universe <- receiveData gameConnection
  atomically $ writeTVar gameUniverse universe

-- | Draw the current state of the 'Universe'.
renderGame :: GameState -> IO Picture
renderGame GameState{..} = renderUniverse <$> readTVarIO gameUniverse

-- | This does nothing since updates come from server.
-- See 'handleUpdates'.
updateGame :: Float -> GameState -> IO GameState
updateGame _dt gs = return gs

main :: IO ()
main = do
  universe <- atomically $ newTVar emptyUniverse
  runClient "localhost" 8000 "/connect" $ \conn -> do
    let gs = GameState universe conn
    _ <- forkIO (handleUpdates gs)
    playIO display bgColor fps gs renderGame handleGame updateGame
  where
    winOffset = (100, 100)
    display   = InWindow "Game of Snakes" (w, h) winOffset
    bgColor   = black
    fps       = 60

    (fieldWidth, fieldHeight) = fieldSize
    (w, h) = (floor fieldWidth, floor fieldHeight)
