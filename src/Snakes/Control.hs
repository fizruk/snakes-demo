{-# LANGUAGE RecordWildCards #-}
module Snakes.Control where

import qualified Data.Map as Map
import Graphics.Gloss
import Snakes.Model

-- | Possible user actions.
data Action
  = RedirectSnake Point   -- ^ Point snake's head in a different direction.

handleAction :: Action -> Snake -> Snake
handleAction (RedirectSnake pos) snake = snake { snakeTarget = Just pos }

handlePlayerAction :: PlayerName -> Action -> Universe -> Universe
handlePlayerAction name action u@Universe{..} = u { uSnakes = Map.adjust (handleAction action) name uSnakes }
