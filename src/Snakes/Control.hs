{-# LANGUAGE RecordWildCards #-}
module Snakes.Control where

import qualified Data.Map as Map
import Graphics.Gloss
import Snakes.Model

-- | Possible user actions.
data SnakeAction
  = SnakeRedirect Point   -- ^ Point snake's head in a different direction.

-- | Apply 'SnakeAction' to a 'Snake'.
applySnakeAction :: SnakeAction -> Snake -> Snake
applySnakeAction (SnakeRedirect pos) snake = snake { snakeTarget = Just pos }

-- | Apply 'SnakeAction' to player's 'Snake'.
handleSnakeAction :: PlayerName -> SnakeAction -> Universe -> Universe
handleSnakeAction name action u@Universe{..}
  = u { uSnakes = Map.adjust (applySnakeAction action) name uSnakes }

